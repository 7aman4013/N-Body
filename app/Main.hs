module Main where

import qualified Analysis as Analysis
import qualified Simulation as Simulation
import System.IO (hFlush, stdout)

---- This file is not very useful for the user. Please take a look at Variables.hs!

main :: IO ()
main = do
    putStrLn "Welcome to the N-body Simulator!"
    simChoice <- promptChoice

    case simChoice of 
        "simulation" -> Simulation.main
        "analysis" -> Analysis.main

promptChoice :: IO String
promptChoice = do
  putStrLn "Please choose simulation type: 'simulation' or 'analysis'"
  putStr "Your choice: "
  hFlush stdout
  input <- getLine
  if input `elem` ["simulation", "analysis"]
    then return input
    else do
      putStrLn "Sorry, that is not a valid input. Please try again."
      promptChoice -- Try again if invalid