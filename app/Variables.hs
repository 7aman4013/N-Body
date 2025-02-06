module Variables where

import Data.Vector (Vector, fromList, toList)
import Data.Fixed

------ This simply exists to resolve the cyclical dependency of Main.


------ Time complexity scaling:
--- Number of balls
-- Direct Sum: O(n^2)
-- Barnes-Hutt: O(n log n)
---- Number of gravity functions: n * functions (still O(n))
---- Order of integration: n * order (still O(n))

---- Graphics-related variables
-- Please be sure not to make these too big, as the visual effect of wrapping around might not work.
screenWidth, screenHeight :: Float
screenWidth = 700
screenHeight = 700

---- Simulation-graphics related variables:
screenFPS :: Int
screenFPS = 60

includeOutlines :: Bool
includeOutlines = True

outlineThickness :: Float
outlineThickness = 0.05 -- Outline of a ball as a percentage of its radius

---- Simulation-related variables
temperatureLossRate :: Float
temperatureLossRate = 0.0 -- Decay in temperature of a ball per update as a percentage

speedLossRate :: Float
speedLossRate = 0.0 -- Decay in speed of a ball per update as a percentage

heatTransferFactor :: Float
heatTransferFactor = 0.5 -- Ratio of temperature exchanged to total temperature per collision as a percentage

demos :: [World]
demos = map fromList [
  --- demo 0
  [
    Ball (100,0) (0,0) 40 30 4500 [gravity],
    Ball (-100,0) (0,0) 40 30 0 [gravity]],

  --- demo 1
  [
    Ball (100,0) (0,0) 40 30 4500 [gravity],
    Ball (0,200) (0,0) 40 30 0 [gravity],
    Ball (-200,0) (0,0) 40 30 4500 [gravity]],

  --- demo 2
  ring (0, 0) (0, -25) 100 1 2.5 500 [] 30
  ++ [ Ball (0, 0) (0,0) 100 60 3000 [clockwiseStrong]],

  --- demo 3
  ring (0, 0) (0,0) 100 5 10 3000 [clockwiseStrong] 10,

  --- demo 4
  ring (0, 0) (0,0) 100 5 10 2000 [heatGrav] 2
  ++ ring (100, 100) (0,0) 100 5 10 100 [heatGrav] 2,

  --- demo 5
  ring (0, 0) (0, 0) 100 7.5 5 500 [gravity, clockwiseStrong] 30,

  --- demo 6
  [Ball (100,100) (100,100) 2 15 7000 [spiral],
  Ball (100,-100) (100,-100) 2 10 3000 [spiral],
  Ball (-100,100) (-100,100) 2 5 1000 [spiral],
  Ball (-100,-100) (-100,-100) 2 20 20000 [spiral]],

  --- demo 7
  ring (0, 0) (0,0) 75 10 7 2000 [gravity] 6
  ++ ring (50, -50) (-30,0) 75 10 7 100 [negGravity] 6
  ++ ring (-150, -150) (0,30) 75 10 7 2000 [gravity] 6
  ++ ring (200, 200) (-15,15) 75 10 7 100 [negGravity] 6,

  -- demo 8 
  square (0,0) (10,-10) 50 10 10 2000 (counterClockwiseStrong : replicate 5 gravity),

  -- demo 9 {PLEASE DO NOT RUN THIS AS A VISUALIZED SIMULATION. IT *WILL* LAG. A LOT.}
  ring (0,0) (0,0) 300 10 0.25 1000 [gravity] 100

  -- demo 10? demo 11? demo 154,915,122? Who knows! It's up to you :)
  ]

ring :: (Float, Float) -> (Float, Float) -> Float -> Float -> Float -> Float -> [GravityFunc] -> Int -> [Ball]
ring pos vel radius mass rad temp f count =
  [ 
    Ball (wrapPos (pos +: (cos angle, sin angle) *: radius)) vel mass rad temp f
    | i <- [0 .. count - 1],
      let angle = 2 * pi * fromIntegral i / fromIntegral count
  ]

square :: (Float, Float) -> (Float, Float) -> Float -> Float -> Float -> Float -> [GravityFunc] -> [Ball]
square (x,y) vel len mass rad temp f = 
  [
  Ball (wrapPos (x+len,y+len)) vel mass rad temp f,
  Ball (wrapPos (x+len,y-len)) vel mass rad temp f,
  Ball (wrapPos (x-len,y+len)) vel mass rad temp f,
  Ball (wrapPos (x-len,y-len)) vel mass rad temp f
  ]

data Ball = Ball
  { position :: (Float, Float),
    velocity :: (Float, Float),
    mass :: Float,
    radius :: Float,
    temperature :: Float,
    gravityFuncs :: [GravityFunc]
  }

instance Eq Ball where
  (==) ball1 ball2 = position ball1 == position ball2

instance Show Ball where
  show ball = show $ position ball

type World = Vector Ball




type GravityFunc = Ball -> Ball -> (Float, Float)

gravity :: GravityFunc
gravity ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (fx, fy)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2)
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)
    force = -400 * (mass ball1 * mass ball2) / (dist ^ 2)
    fx = force * dx / dist
    fy = force * dy / dist

negGravity :: GravityFunc
negGravity ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (gravity ball1 ball2) *: (-1)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2)
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)

clockwise :: GravityFunc
clockwise ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (fx, fy)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2)
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)
    theta = atan2 dy dx
    ct = cos theta
    st = sin theta
    r = (-mass ball1 * mass ball2) / (dist ^ 2)
    tf = (-mass ball1 * mass ball2) / dist

    fx = (r * ct - tf * st) / 5
    fy = (r * st + tf * ct) / 5

counterClockwise :: GravityFunc
counterClockwise ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (fx, fy)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2) 
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)
    theta = atan2 dy dx
    ct = cos theta
    st = sin theta
    r = (-mass ball1 * mass ball2) / (dist ^ 2)
    tf = (-mass ball1 * mass ball2) / dist

    fx = (r * ct + tf * st) / 5
    fy = (r * st - tf * ct) / 5

clockwiseStrong :: GravityFunc
clockwiseStrong ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (clockwise ball1 ball2) *: 100
  | otherwise = (0, 0)
    where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2) 
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)

counterClockwiseStrong :: GravityFunc
counterClockwiseStrong ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (counterClockwise ball1 ball2) *: 100
  | otherwise = (0, 0)
    where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2) 
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)

spiral :: GravityFunc
spiral ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (fx, fy)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2) 
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)
    theta = atan2 dy dx
    ct = cos theta
    st = sin theta
    r = -5 * 2 ** (1 / (2 * pi) * (-theta + (3 * pi) / 2)) * (mass ball1 * mass ball2)
    tf = (-mass ball1 * mass ball2)

    fx = r * ct - tf * st
    fy = r * st + tf * ct

heatGrav :: GravityFunc
heatGrav ball1 ball2
  | boundsCloseEnough = (0, 0)
  | dist > radius ball1 + radius ball2 = (fx, fy)
  | otherwise = (0, 0)
  where
    (dist, dx, dy) = wrapDistance (position ball1) (position ball2)
    boundsCloseEnough =
      abs (dx) < (radius ball1 + radius ball2)
        && abs (dy) < (radius ball1 + radius ball2)
    force = -3 * ((temperature ball1 - 1000) * (temperature ball2 - 1000)) / (dist ^ 2)
    fx = force * dx / dist
    fy = force * dy / dist


-- Apply wrap-around for a position to simulate a toroidal world
wrapPos :: (Float, Float) -> (Float, Float)
wrapPos (x, y) = (modPos x screenWidth, modPos y screenHeight)
  where
    modPos v maxDim = (v + maxDim / 2) `mod'` maxDim - maxDim / 2

-- Calculate wrapped distance and direction from ball1 to ball2
wrapDistance :: (Float, Float) -> (Float, Float) -> (Float, Float, Float)
wrapDistance p1 p2 = (sqrt (dx * dx + dy * dy), dx, dy)
  where
    (x1, y1) = wrapPos p1
    (x2, y2) = wrapPos p2
    dx1 = x2 - x1
    dx2 = if (dx1 > 0) then dx1 - screenWidth else dx1 + screenWidth
    dx = if (abs dx1 < abs dx2) then dx1 else dx2
    dy1 = y2 - y1
    dy2 = if (dy1 > 0) then dy1 - screenHeight else dy1 + screenHeight
    dy = if (abs dy1 < abs dy2) then dy1 else dy2

  
-- Helper functions for vector operations
infixl 6 +:
infixl 6 -:
infixl 7 *:
infixl 7 /:

(+:) :: (Float, Float) -> (Float, Float) -> (Float, Float)
(x1, y1) +: (x2, y2) = (x1 + x2, y1 + y2)

(-:) :: (Float, Float) -> (Float, Float) -> (Float, Float)
(x1, y1) -: (x2, y2) = (x1 - x2, y1 - y2)

vecAbs :: (Float, Float) -> Float
vecAbs (x, y) = sqrt(x*x + y*y)

(*:) :: (Float, Float) -> (Float) -> (Float, Float)
(x, y) *: r = (r * x, r * y)

(/:) :: (Float, Float) -> (Float) -> (Float, Float)
(x, y) /: r
    | r == 0 = error "Division by zero in vecDiv!"
    | otherwise = (x / r, y / r)
