module Analysis where

import Simulation hiding (runSimulation, screenWidth, screenHeight)
import Variables
import Data.Vector (Vector, (!), toList)
import Data.Fixed
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color (makeColor) 
import Control.Parallel.Strategies (parMap, rseq) 
import Control.DeepSeq (force, NFData)
import System.Clock (getTime, Clock(Monotonic), TimeSpec(..), sec, nsec)
import Numeric (showFFloat)
import System.IO (hFlush, stdout)


-- New data type to store simulation results
data SimulationResult = SimulationResult {
    timePoints :: [Float],
    energyValues :: [Float],
    methodName :: String
}

data IntegrationWName = IntegrationWName {
  integral :: Integration,
  name :: String
}

instance Show IntegrationWName where
  show = name


-- Run simulation for a specific integration method
runSimulation :: Float -> Int -> World -> IntegrationWName -> SimulationResult
runSimulation dt steps initialWorld integrate@IntegrationWName{integral = integ, name = name} = 
    SimulationResult times energies name
  where
    times = [0, dt..dt * fromIntegral steps]
    worlds = iterate (simulationStep integ dt) initialWorld
    --energies = map calculateEnergy $ take (steps + 1) worlds
    energies = parMap rseq calculateEnergy $ take (steps + 1) worlds

data PlotDimensions = PlotDimensions {
    windowWidth :: Int,
    windowHeight :: Int,
    plotWidth :: Float,
    plotHeight :: Float,
    marginLeft :: Float,
    marginRight :: Float,
    marginTop :: Float,
    marginBottom :: Float
}

---- Helper function to convert hue to RGB
-- Why does Gloss not export hue???
-- Uses the standard HSV to RGB conversion algorithm
hueToRGB :: Float -> Color
hueToRGB h = makeColor r g b 1.0
  where
    h' = if h < 0 then h + 360 else if h >= 360 then h - 360 else h
    c = 1.0  -- Chroma (using maximum saturation and value)
    x = c * (1 - abs ((h' / 60) `mod'` 2 - 1))
    m = 0.0  -- Lightness adjustment
    
    (r', g', b') = case floor (h' / 60) of
        0 -> (c, x, 0)
        1 -> (x, c, 0)
        2 -> (0, c, x)
        3 -> (0, x, c)
        4 -> (x, 0, c)
        5 -> (c, 0, x)
        _ -> (0, 0, 0)  -- This case should never happen with normalized h'
    
    r = r' + m
    g = g' + m
    b = b' + m

-- Generate evenly spaced colors on the hue wheel
generateColors :: Int -> [Color]
generateColors n
    | n <= 0    = []
    | n == 1    = [hueToRGB 0]
    | otherwise = [hueToRGB (fromIntegral i * (360 / fromIntegral n)) | i <- [0.. (n - 1)]]


createPlot :: PlotDimensions -> (String, String, String) -> [(String, [(Float, Float)])] -> Picture
createPlot dims (plotTitle, xLabel, yLabel) datasets =
    pictures [
        plotBackground,
        plotAxes,
        plotGridLines,
        plotData,
        plotLegend,
        plotLabels
      ]
  where
    -- Assign evenly spaced colors to datasets based on angle around the color wheel
    colors = generateColors (length datasets)
    datasetsWithColors = zipWith (\(name, points) col -> (name, col, points)) datasets colors

    -- Find data ranges for scaling
    allPoints = concatMap (\(_, _, points) -> points) datasetsWithColors
    allX = map fst allPoints
    allY = map snd allPoints
    maxX = maximum (0.0001 : allX)
    maxY = maximum (0.0001 : allY)
    minX = minimum allX
    minY = minimum allY
    xRange = maxX - minX
    yRange = maxY - minY

    -- Scale factors to fit in plot area
    xScale = (plotWidth dims - marginLeft dims - marginRight dims) / xRange
    yScale = (plotHeight dims - marginTop dims - marginBottom dims) / yRange

    -- Transform data coordinates to screen coordinates
    transformPoint :: (Float, Float) -> (Float, Float)
    transformPoint (x, y) = (
        (x - minX) * xScale + marginLeft dims,
        (y - minY) * yScale + marginBottom dims)

    -- Background
    plotBackground = color (makeColor 0.1 0.1 0.1 1.0) $
        translate (-fromIntegral (windowWidth dims) / 2) (-fromIntegral (windowHeight dims) / 2) $
        polygon [(0, 0), 
                (fromIntegral $ windowWidth dims, 0), 
                (fromIntegral $ windowWidth dims, fromIntegral $ windowHeight dims), 
                (0, fromIntegral $ windowHeight dims)]

    -- Axes with ticks
    plotAxes = translate (-fromIntegral (windowWidth dims) / 2) (-fromIntegral (windowHeight dims) / 2) $
        color white $ pictures [
        -- X axis
        line [(marginLeft dims, marginBottom dims), 
              (plotWidth dims - marginRight dims, marginBottom dims)],
        -- Y axis
        line [(marginLeft dims, marginBottom dims), 
              (marginLeft dims, plotHeight dims - marginTop dims)],
        -- X axis ticks and labels
        pictures [
            let x = minX + t * xRange / 5
                screenX = fst $ transformPoint (x, 0)
            in pictures [
                line [(screenX, marginBottom dims - 5), 
                      (screenX, marginBottom dims + 5)],
                translate (screenX - 10) (marginBottom dims - 20) $ 
                    scale 0.1 0.1 $ text (showFFloat (Just 2) x "")
            ] | t <- [0..5]
        ],
        -- Y axis ticks and labels
        pictures [
            let y = minY + t * yRange / 5
                screenY = snd $ transformPoint (0, y)
            in pictures [
                line [(marginLeft dims - 5, screenY), 
                      (marginLeft dims + 5, screenY)],
                translate (marginLeft dims - 60) (screenY - 5) $ 
                    scale 0.1 0.1 $ text (showFFloat (Just 2) y "")
            ] | t <- [0..5]
        ]
      ]

    -- Grid lines
    plotGridLines = translate (-fromIntegral (windowWidth dims) / 2) (-fromIntegral (windowHeight dims) / 2) $
        color (greyN 0.2) $ pictures [
        pictures [
            let x = minX + t * xRange / 5
                screenX = fst $ transformPoint (x, 0)
            in line [(screenX, marginBottom dims), 
                    (screenX, plotHeight dims - marginTop dims)] | t <- [0..5]
        ],
        pictures [
            let y = minY + t * yRange / 5
                screenY = snd $ transformPoint (0, y)
            in line [(marginLeft dims, screenY), 
                    (plotWidth dims - marginRight dims, screenY)] | t <- [0..5]
        ]
      ]

    -- Plot the actual data
    plotData = translate (-fromIntegral (windowWidth dims) / 2) (-fromIntegral (windowHeight dims) / 2) $
        pictures [color col $ line $ map transformPoint points | (_, col, points) <- datasetsWithColors]
    
    -- Legend
    plotLegend = translate (-fromIntegral (windowWidth dims) / 2 + plotWidth dims - marginRight dims + 20) 
                          (-fromIntegral (windowHeight dims) / 2 + plotHeight dims - marginTop dims - 100) $
        pictures $ zipWith (\(name, col, _) y -> 
            pictures [
                color col $ translate 0 y $ line [(0, 0), (20, 0)],
                color white $ translate 30 y $ scale 0.1 0.1 $ text name
            ]) datasetsWithColors [0, -20..]

    -- Labels and title
    plotLabels = translate (-fromIntegral (windowWidth dims) / 2) (-fromIntegral (windowHeight dims) / 2) $
        color white $ pictures [
            translate (plotWidth dims / 2) (marginBottom dims / 2) $ 
                scale 0.15 0.15 $ text xLabel,
            translate (marginLeft dims / 2) (plotHeight dims / 2) $ 
                rotate 90 $ scale 0.15 0.15 $ text yLabel,
            translate (plotWidth dims / 2) (plotHeight dims - marginTop dims / 2) $
                scale 0.2 0.2 $ text plotTitle
          ]

createEnergyPlot :: PlotDimensions -> [SimulationResult] -> Picture
createEnergyPlot dims results =
    createPlot dims ("Energy Conservation", "Time", "Energy") datasets
  where
    datasets = map (\result -> 
      (methodName result, 
      zip (timePoints result) (energyValues result)))
       results

-- Calculate total energy of the system
calculateEnergy :: World -> Float
calculateEnergy world = kineticEnergy + potentialEnergy
  where
    balls = toList world
    -- Kinetic energy
    kineticEnergy = sum [0.5 * mass b * (vecAbs (velocity b))^2 | b <- balls]
    -- Potential energy (from gravitational interactions)
    potentialEnergy = sum [calculatePairPotential b1 b2 | 
                          (i, b1) <- zip [0..] balls,
                          b2 <- drop (i+1) balls]

-- Calculate gravitational potential between two balls
calculatePairPotential :: Ball -> Ball -> Float
calculatePairPotential b1 b2 = sum [calcPotential gFunc b1 b2 | gFunc <- gravityFuncs b1]
  where
    calcPotential gFunc ball1 ball2 = 
        let (dist, _, _) = wrapDistance (position ball1) (position ball2)
            (fx, fy) = gFunc ball1 ball2
            potential = -dist * sqrt(fx*fx + fy*fy)
        in potential

-- Modified error calculation function to use percentages
calculatePercentError :: SimulationResult -> SimulationResult -> ([Float], [Float])
calculatePercentError control result = 
    let controlEnergies = interpolateEnergies (timePoints control) (energyValues control) (timePoints result)
        errors = zipWith (\e1 e2 -> 100 * abs (e1 - e2) / abs e2) (energyValues result) controlEnergies
    in (timePoints result, errors)
      
-- Helper function to interpolate energy values to match time points
interpolateEnergies :: [Float] -> [Float] -> [Float] -> [Float]
interpolateEnergies controlTimes controlEnergies resultTimes =
    map (interpolateValue controlTimes controlEnergies) resultTimes

-- Linear interpolation helper
interpolateValue :: [Float] -> [Float] -> Float -> Float
interpolateValue times values t =
    case span (<= t) times of
        ([], _) -> head values
        (_, []) -> last values
        (lower, upper) ->
            let i = length lower - 1
                t1 = times !! i
                t2 = times !! (i + 1)
                v1 = values !! i
                v2 = values !! (i + 1)
                ratio = (t - t1) / (t2 - t1)
            in v1 + ratio * (v2 - v1)

createEnergyErrorPlot :: PlotDimensions -> [SimulationResult] -> Picture
createEnergyErrorPlot dims (control:results) =
    createPlot dims ("Energy Error Analysis", "Time", "Error vs Control (%)") datasets
  where
    datasets = map (\result -> 
        let (times, errors) = calculatePercentError control result
        in (methodName result, zip times errors)
      ) results

-- Secondary main method + its helpers
main :: IO ()
main = do
    putStrLn "Welcome to Energy Analysis Mode!"
    putStrLn "Choose a demo world to simulate:"
    listDemos demos
    worldChoice <- promptDemoChoice (length demos)
    
    putStrLn "Enter the base timestep size:"
    dtStr <- getLine
    let dt = read dtStr :: Float
    
    putStrLn "Enter the number of steps:"
    stepsStr <- getLine
    let steps = read stepsStr :: Int
    
    --putStrLn "Enter window width (pixels):"
    --widthStr <- getLine
    --let width = read widthStr :: Int
    
    --putStrLn "Enter window height (pixels):"
    --heightStr <- getLine
    --let height = read heightStr :: Int

    let (width,height) = (round(screenWidth),round(screenHeight))
    
    let dims = PlotDimensions {
        windowWidth = width,
        windowHeight = height,
        plotWidth = fromIntegral width * 0.8,
        plotHeight = fromIntegral height * 0.8,
        marginLeft = fromIntegral width * 0.15,
        marginRight = fromIntegral width * 0.05,
        marginTop = fromIntegral height * 0.1,
        marginBottom = fromIntegral height * 0.1
    }

    ---- Control has steps that are x times as small as the experiment group.
    putStrLn "Input control factor (e.g. 5):"
    controlFactor <- getLine
    let control = read controlFactor :: Int
    
    let world = demos !! worldChoice
    let controlSteps = steps * control
    let controlDt = dt / (fromIntegral control)

    let runC = runSimulation controlDt controlSteps world
    let run = runSimulation dt steps world

    controlChoice <- promptControl
    let controlIntegration = 
            case controlChoice of
                "euler" -> eulerI {name = "Euler (Control)"}
                "rk4" -> rungekutta4I {name = "Runge-Kutta 4 (Control)"}
                "verlet" -> verletI {name = "Verlet (Control)"}
                "leapfrog" -> leapfrogI {name = "Leapfrog (Control)"}
                "yoshida" -> yoshidaI {name = "Yoshida (Control)"}
    
    --{--
    let results = [
            runC controlIntegration,
            run eulerI,
            run rungekutta4I,
            run verletI,
            run leapfrogI,
            run yoshidaI
          ]
    --}

    plotChoice <- promptPlot

    case plotChoice of
        "energy" -> 
                display
                    (InWindow "Energy Analysis" (width, height) (100, 100))
                    black
                    (createEnergyPlot dims results)
        "error" -> 
                display
                    (InWindow "Energy Error Analysis" (width, height) (100, 100))
                    black
                    (createEnergyErrorPlot dims results)
--}

promptPlot :: IO String
promptPlot = do
  putStrLn "Please choose what you want to plot: 'energy' or 'error'"
  putStr "Your choice: "
  hFlush stdout
  input <- getLine
  if input `elem` ["energy", "error"]
    then return input
    else do
      putStrLn "Sorry, that is not a valid input. Please try again."
      promptPlot -- Try again if invalid

promptControl :: IO String
promptControl = do
  putStrLn "Choose numerical method as control: 'euler', 'rk4', 'verlet', 'leapfrog', or 'yoshida'."
  putStr "Your choice: "
  hFlush stdout
  input <- getLine
  if input `elem` ["euler", "rk4", "leapfrog", "verlet", "yoshida"]
    then return input
    else do
      putStrLn "Sorry, that is not a valid input. Please try again."
      promptMethod -- Try again if invalid

eulerI :: IntegrationWName
eulerI = IntegrationWName euler "Euler"

rungekutta4I :: IntegrationWName
rungekutta4I = IntegrationWName rungekutta "Runge-Kutta 4"

verletI :: IntegrationWName
verletI = IntegrationWName verlet "Verlet"

leapfrogI :: IntegrationWName
leapfrogI = IntegrationWName leapfrog "Leapfrog"

yoshidaI :: IntegrationWName
yoshidaI = IntegrationWName yoshida "Yoshida"


