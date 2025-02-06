module Simulation where

import Variables
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Interact hiding (Vector)
import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parMap, rpar)
import System.IO (hFlush, stdout)

---- Rendering
drawBall :: Ball -> Picture
drawBall ball =
  Pictures $
    map drawOutlines
      (wrapAroundPositions (position ball) (radius ball))
  where
    col = makeColor (min 1.0 (temperature ball / 5000)) 0 1 1
    drawAt r col (x, y) = translate x y $ color col $ circleSolid r
    drawOutlines
      | not includeOutlines = drawAt (radius ball) col
      | radius ball <= 4 = drawAt (radius ball) col
      | otherwise = drawAt (radius ball) white <> drawAt ((1 - outlineThickness) * (radius ball)) col

drawWorld :: World -> Picture
drawWorld world = Pictures $ V.toList $ V.map drawBall world

wrapAroundPositions :: (Float, Float) -> Float -> [(Float, Float)]
wrapAroundPositions (x, y) r = [(x', y') | x' <- [x, wrapX], y' <- [y, wrapY]]
  where
    wrapX = if x + r > screenWidth / 2 then x - screenWidth else x + screenWidth
    wrapY = if y + r > screenHeight / 2 then y - screenHeight else y + screenHeight

-- Collision
-- Check and apply collisions for all balls in the world
-- Calculate all collisions first before actually applying any (for fairness of collisions)
applyCollisions :: World -> World
applyCollisions world = runST $ do
  let len = V.length world
  velocityChanges <- MV.replicate len (0, 0)
  temperatureChanges <- MV.replicate len 0
  mWorld <- V.thaw world

  forM_ [0 .. len - 2] $ \i ->
    forM_ [i + 1 .. len - 1] $ \j -> do
      ball1 <- MV.read mWorld i
      ball2 <- MV.read mWorld j
      let (newBall1, newBall2) = collideBalls ball1 ball2

      -- Calculate velocity changes
      let vChange1 = velocity newBall1 -: velocity ball1
      let vChange2 = velocity newBall2 -: velocity ball2

      -- Calculate temperature changes
      let tChange1 = temperature newBall1 - temperature ball1
      let tChange2 = temperature newBall2 - temperature ball2

      -- Accumulate velocity changes
      MV.modify velocityChanges (+: vChange1) i
      MV.modify velocityChanges (+: vChange2) j

      -- Accumulate temperature changes
      MV.modify temperatureChanges (+ tChange1) i
      MV.modify temperatureChanges (+ tChange2) j

  -- Apply accumulated velocity changes to the world
  forM_ [0 .. len - 1] $ \i -> do
    ball <- MV.read mWorld i
    vChange <- MV.read velocityChanges i
    tChange <- MV.read temperatureChanges i
    let newBall =
          ball
            { 
              velocity = velocity ball +: vChange,
              temperature = temperature ball + tChange
            }
    MV.write mWorld i newBall

  -- Freeze the mutable world back into an immutable one
  V.unsafeFreeze mWorld

collideBalls :: Ball -> Ball -> (Ball, Ball)
collideBalls ball1 ball2
  | boundsFarEnough = (ball1, ball2)
  | dist <= radius ball1 + radius ball2 = (newBall1, newBall2)
  | otherwise = (ball1, ball2)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2)
    boundsFarEnough =
      abs (dx) > (radius ball1 + radius ball2)
        && abs (dy) > (radius ball1 + radius ball2)
    (vx1, vy1) = velocity ball1
    (vx2, vy2) = velocity ball2
    m1 = mass ball1
    m2 = mass ball2
    normal = (dx / dist, dy / dist)

    -- Calculate relative velocity along the normal
    relVel = (vx2 - vx1) * fst normal + (vy2 - vy1) * snd normal

    -- Calculate the impulse based on the relative velocity and the normal
    impulse = (2 * relVel * m1 * m2) / (m1 + m2)

    impulseVec1 = normal *: (impulse / m1)
    impulseVec2 = normal *: (impulse / m2)

    -- Calculate new velocities
    -- newVelocity1 = vecAdd (velocityBall1) (impulseVec1)
    -- newVelocity2 = vecSub (velocityBall2) (impulseVec2)
    newVelocity1 = (vx1, vy1) +: impulseVec1
    newVelocity2 = (vx2, vy2) -: impulseVec2

    -- Temperature exchange that respects differences in mass
    -- plus the fact that collisions are basically infitesimally long
    -- Temperature exchange towards equilibrium
    t1 = temperature ball1
    t2 = temperature ball2

    deltaT1 = heatTransferFactor * (t1 - t2) * (m2 / (m1 + m2))
    deltaT2 = heatTransferFactor * (t2 - t1) * (m1 / (m1 + m2))

    -- New temperatures after exchange
    newTemp1 = t1 - deltaT1
    newTemp2 = t2 - deltaT2

    newBall1 = ball1 {velocity = newVelocity1, temperature = newTemp1}
    newBall2 = ball2 {velocity = newVelocity2, temperature = newTemp2}


---- Secondary main method + its helpers
main :: IO ()
main = do
  putStrLn "Welcome to Simulation Mode!"
  putStrLn "Choose a demo world to simulate:"
  listDemos demos -- Display the available demos
  worldChoice <- promptDemoChoice (length demos) -- Get the user's demo choice

  method <- promptMethod
  case method of
      "euler" -> runSimulation "Direct Summation (Euler's)" simulationStep euler (demos !! worldChoice)
      "rk4" -> runSimulation "Direct Summation (Runge-Kutta-4)" simulationStep rungekutta (demos !! worldChoice)
      "verlet" -> runSimulation "Direct Summation (Verlet)" simulationStep verlet (demos !! worldChoice)
      "leapfrog" -> runSimulation "Direct Summation (Leapfrog)" simulationStep leapfrog (demos !! worldChoice)
      "yoshida" -> runSimulation "Direct Summation (Yoshida)" simulationStep yoshida (demos !! worldChoice)

listDemos :: [World] -> IO ()
listDemos demos = do
  putStrLn "Available demo worlds:"
  mapM_ (\(i, _) -> putStrLn $ "Demo: " ++ show i) (zip [0 ..] demos)

promptDemoChoice :: Int -> IO Int
promptDemoChoice demoCount = do
  putStrLn "Enter the number of the demo world you want to simulate:"
  hFlush stdout
  input <- getLine
  case reads input of
    [(choice, "")] | choice >= 0 && choice < demoCount -> return choice -- Valid choice
                   | otherwise -> do
                       putStrLn "Invalid choice. Please choose a valid demo number."
                       promptDemoChoice demoCount -- Retry if invalid
    _ -> do
      putStrLn "Sorry, that is not a valid input. Please input a number."
      promptDemoChoice demoCount -- Retry if invalid

promptMethod :: IO String
promptMethod = do
  putStrLn "Choose numerical method: 'euler', 'rk4', 'verlet', 'leapfrog', or 'yoshida'."
  putStr "Your choice: "
  hFlush stdout
  input <- getLine
  if input `elem` ["euler", "rk4", "leapfrog", "verlet", "yoshida"]
    then return input
    else do
      putStrLn "Sorry, that is not a valid input. Please try again."
      promptMethod -- Try again if invalid

-- Run the simulation by passing the selected integration method
runSimulation :: String -> (Integration -> Float -> World -> World) -> Integration -> World -> IO ()
runSimulation title simulation integrateMethod world = do
  putStrLn $ "Running " ++ title ++ " simulation..."
  play
    (InWindow "N-body Simulation" (round screenWidth, round screenHeight) (100, 100))
    black
    screenFPS
    world
    drawWorld
    (const id)
    (simulation integrateMethod)
    
---- Order matters here, in order to handle simultaneous calculations
-- We store updated particles in a separate independent structure that is 
-- only written into and cannot interfere with integration at all.
simulationStep :: Integration -> Float -> World -> World
simulationStep integrate dt world = 
    -- Create the updated world using the original world for integration
    applyCollisions $ V.map (\(b, acc) -> integrate dt acc b world) (V.zip world accBalls)
  where
    -- Calculate accelerations using the original world
    accBalls = V.map (\b -> getAcceleration b world) world

    
---- Acceleration function of a given ball vs. the entire world
-- E.g. It is the target of all other balls' gravity functions
getAcceleration :: Ball -> World -> (Float, Float)
getAcceleration ball world = 
    foldl (+:) (0, 0) [(func ball other) *: (-1) | 
    other <- V.toList world, 
    other /= ball, 
    func <- gravityFuncs other]

---- Acceleration function given a different position
accFunc :: (Float, Float) -> Ball -> World -> (Float, Float)
accFunc pos ball world = 
    getAcceleration (ball {position = wrapPos (pos)}) world

---- General integration function that takes an initial acceleration vector
-- and computes the new position and velocity of a given ball
type Integration = Float -> (Float, Float) -> Ball -> World -> Ball

-- 1st order Euler integration
euler :: Integration
euler dt a ball@Ball{position = x, velocity = v} world = newBall
 where 
    newVelocity = v +: a *: dt
    newPosition = wrapPos $ x +: v *: dt

    newBall = ball {position = newPosition, velocity = newVelocity *: (1 - speedLossRate)}

-- 4th order Runge-Kutta integration
rungekutta :: Integration
rungekutta dt a ball@Ball{position = x, velocity = v} world = newBall
 where 
    v1 = a
    x1 = wrapPos $ v

    v2 = acc (x +: x1 *: dt /: 2)
    x2 = wrapPos $ v +: v1 *: dt /: 2

    v3 = acc (x +: x2 *: dt /: 2)
    x3 = wrapPos $ v +: v2 *: dt /: 2

    v4 = acc (x +: x3 *: dt)
    x4 = wrapPos $ v +: v3 *: dt

    vF = v +: (v1 +: v2 *: 2 +: v3 *: 2 +: v4) *: (dt / 6)
    xF = x +: (x1 +: x2 *: 2 +: x3 *: 2 +: x4) *: (dt / 6)
    

    acc pos = accFunc (wrapPos pos) ball world
    newBall = ball {position = xF, velocity = vF *: (1 - speedLossRate)}

-- Velocity Verlet integration (2nd order)
verlet :: Integration
verlet dt a ball@Ball{position = x, velocity = v} world = newBall
  where
    xNext = wrapPos $ x +: v *: dt +: a *: (dt^2 / 2)
    aNext = acc xNext
    vNext = v +: (a +: aNext) *: (dt / 2)

    acc pos = accFunc pos ball world
    newBall = ball {position = xNext, velocity = vNext}

-- Leapfrog integration (2nd order)
leapfrog :: Integration
leapfrog dt a ball@Ball{position = x, velocity = v} world = newBall
 where 
    newPosition = wrapPos $ x +: v *: dt +: a *: (dt^2) /: 2
    newVelocity = v +: (a +: acc(newPosition)) *: dt /: 2

    acc pos = accFunc (wrapPos pos) ball world
    newBall = ball {position = newPosition, velocity = newVelocity *: (1 - speedLossRate)}

-- 4th order Yoshida Integrator
yoshida :: Integration
yoshida dt a ball@Ball{position = x, velocity = v} world = newBall
 where 
    x1 = wrapPos $ x +: v *: (c1 * dt)
    v1 = v +: (acc x1) *: (d1 * dt)

    x2 = wrapPos $ x1 +: v1 *: (c2 * dt)
    v2 = v1 +: (acc x2) *: (d2 * dt)

    x3 = wrapPos $ x2 +: v2 *: (c3 * dt)
    v3 = v2 +: (acc x3) *: (d3 * dt)

    x4 = wrapPos $ x3 +: v3 *: (c4 * dt)
    --v4 = v3 --d4 = 0 so no new term
    v4 = v3 +: (acc x4) *: (d4 * dt)

    -- Weird constants!
    w0 = -1.70241438392
    w1 = 1.35120719196
    c1 = 0.67560359598
    c2 = -0.17560359598
    c3 = c2
    c4 = c1
    d1 = w1
    d2 = w0
    d3 = d1
    d4 = 0

    acc pos = accFunc (wrapPos pos) ball world 
    newBall = ball {position = x4, velocity = v4 *: (1 - speedLossRate)}


{- AN EARNEST ATTEMPT AT A BARNES-HUT METHOD, DESPITE THE IRREGULARITY THAT THIS PROGRAM CONTAINS. IT DOES NOT WORK. 

-- Barnes-Hut needs its integration baked in, for simplicity's sake.
simulationStepBarnesHut :: Float -> Float -> World -> World
simulationStepBarnesHut theta dt world = runST $ do
  let listWorld = V.toList world

  -- Step 1: Create the initial 'world tree' that contains all other trees inside it.
  let quadTree = foldl (\tree ball -> insertBall ball tree) (Empty worldBounds) listWorld
  
  -- Step 2: Apply each ball's forces onto the quadtree
  let forceTree = foldl (\tree ball -> insertForce theta dt ball tree) quadTree listWorld

  -- Step 3: Apply the accumulated forces from the quadtree to each quadrant recursively
  let finalTree = foldForces dt (0, 0) forceTree

  -- Step 4: Take the tree and make a new 'world' out of it.
  let updatedWorld = V.fromList (extractBalls finalTree)

  return updatedWorld

data Tree
  = Empty {bounds :: ((Float, Float), (Float, Float))} -- span of empty quadrant of quadtree
  | Leaf {ball :: Ball, bounds :: ((Float, Float), (Float, Float))} -- span of given leaf quadrant of quadtree
  | Node
      { nw :: Tree,
        ne :: Tree,
        sw :: Tree,
        se :: Tree,
        centerMass :: (Float, Float),
        totalMass :: Float,
        totalForce :: (Float, Float),
        bounds :: ((Float, Float), (Float, Float)) -- (total span of given quadtree)
      }

instance Show Tree where
  show tree = show (extractBalls tree)


worldBounds :: ((Float, Float), (Float, Float))
worldBounds = ((-screenWidth / 2, -screenHeight / 2), (screenWidth / 2, screenHeight / 2))

insertBall :: Ball -> Tree -> Tree
-- Case 1: Inserting ball into empty region -> simply insert it
insertBall ball (Empty bnd) = Leaf ball bnd
-- Case 2: Inserting ball into leaf -> split region into tree and try again
insertBall ball1 (Leaf ball2 bnd) 
  | ball1 == ball2 = (Leaf ball2 bnd)
  | otherwise = 
    let tree = makeQuad bnd
    in insertBall ball1 $ insertBall ball2 tree
-- Case 3: Inserting ball into tree -> recursively search for appropriate placement 
insertBall ball node@(Node {bounds = ((x_min, y_min), (x_max, y_max)), centerMass = cm, totalMass = m}) =
  case quadrant ball (bounds node) of
    "NW" -> node {nw = insertBall ball (nw node), centerMass = newCenter, totalMass = newTotalMass}
    "NE" -> node {ne = insertBall ball (ne node), centerMass = newCenter, totalMass = newTotalMass}
    "SW" -> node {sw = insertBall ball (sw node), centerMass = newCenter, totalMass = newTotalMass}
    "SE" -> node {se = insertBall ball (se node), centerMass = newCenter, totalMass = newTotalMass}
  where
    quadrant Ball {position = (x, y)} ((x_min, x_max),  (y_min, y_max)) =
      if x < (x_min + x_max) / 2
        then if y < (y_min + y_max) / 2 then "NW" 
          else "SW"
        else if y < (y_min + y_max) / 2 then "NE" 
          else "SE"
    newTotalMass = mass ball + m
    newCenter = getCenterOfMass ball (Ball {position = cm, velocity = (0, 0), mass = m, radius = 0, temperature = 0, gravityFuncs = []})

extractBalls :: Tree -> [Ball]
extractBalls (Empty _) = []
extractBalls (Leaf ball _) = [ball]
extractBalls node = extractBalls (nw node) ++ extractBalls (ne node) ++ extractBalls (sw node) ++ extractBalls (se node)

getCenterOfMass :: Ball -> Ball -> (Float, Float)
getCenterOfMass ball1@Ball{position = p1, mass = m1} ball2ball1@Ball{position = p2, mass = m2} = 
  (p1 *: m1 +: p2 *: m2) /: (m1 + m2)

-- Transforms an empty/leaf quadrant into a full node
makeQuad :: ((Float, Float), (Float, Float)) -> Tree
makeQuad ((x_min, y_min), (x_max, y_max)) = Node {
    nw = Empty ((x_min, y_mid), (x_mid, y_max)),
    ne = Empty ((x_mid, y_mid), (x_max, y_max)),
    sw = Empty ((x_min, y_min), (x_mid, y_mid)),
    se = Empty ((x_mid, y_min), (x_max, y_mid)),
    totalMass = 0, centerMass = (0,0), totalForce = (0,0),
    bounds = ((x_min, y_min), (x_max, y_max))}
    where 
        (x_mid, y_mid) = ((x_min, y_min) +: (x_max, y_max)) /: 2

-- Apply forces from a ball onto the entire tree
insertForce :: Float -> Float -> Ball -> Tree -> Tree
insertForce _ _ _ (Empty bnd) = (Empty bnd) -- Nothing to apply on
insertForce _ dt ball (Leaf other bnd)
  | ball == other = (Leaf other bnd) -- Dont apply a ball's force on itself...
  | otherwise = Leaf (applyForce dt ball other) bnd
insertForce theta dt ball (node@(Node {centerMass = cm, totalMass = m, totalForce = f1})) =
  if distance / width < theta
    then node {totalForce = f1 +: f2}
    else  -- The root of this tree remains unchanged. Instead, apply the force on each of its nodes.
      node
        { nw = insertForce theta dt ball (nw node),
          ne = insertForce theta dt ball (ne node),
          sw = insertForce theta dt ball (sw node),
          se = insertForce theta dt ball (se node)
        }
  where
    f2 = velocity $ applyForce dt ball (Ball {position = cm, velocity = (0,0), mass = m, radius = 0, temperature = 0, gravityFuncs = []}) -- CoM Ball
    (distance, _, _) = wrapDistance (position ball) cm
    ((x_min, _), (x_max, _)) = bounds node
    width = x_max - x_min

-- Not a very useful function, usually. It's just here because it was hard-coded for Barnes-Hut
applyForce :: Float -> Ball -> Ball -> Ball
applyForce dt source target = target {velocity = v +: f *: (dt / m)}
 where 
  f = foldl (+:) (0, 0) [(func source target) *: (-1) | func <- gravityFuncs source]
  v = velocity target
  m = mass target

-- Given a force vector coming from a parent node, the given node 'absorbs' it
-- and then redistributes its own forces on its children
foldForces :: Float -> (Float, Float) -> Tree -> Tree
foldForces _ _ (Empty bnd) = Empty bnd
foldForces dt f (Leaf ball bnd) = Leaf newBall bnd 
 where 
  newBall = ball {velocity = v +: f *: (dt / m)}
  v = velocity ball
  m = mass ball
foldForces dt f node@(Node {totalForce = f', totalMass = m, nw = nW, ne = nE, sw = sW, se = sE}) =
  let 
    totalF = f +: f'
    -- Recursively divide the force to the children of the node
    newNW = foldForces dt (applyOn nW totalF) nW
    newNE = foldForces dt (applyOn nE totalF) nE
    newSW = foldForces dt (applyOn sW totalF) sW
    newSE = foldForces dt (applyOn sE totalF) sE
  in node {totalForce = (0,0), nw = newNW, ne = newNE, sw = newSW, se = newSE}
  where 
    ---- Function to calculate contribution of child towards total force of parent
    applyOn (Empty _) _ = (0, 0)
    applyOn (Leaf ball _) g = g *: (mass ball / m) 
    applyOn (node@(Node {totalForce = h, totalMass = tm})) g = g *: (tm / m) +: h

-}