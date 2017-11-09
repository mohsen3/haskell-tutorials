{-# LANGUAGE RecordWildCards #-}
module Main where

import Types
import Scheduler
import Simulator

import Control.Applicative
import qualified Data.Heap as H
import qualified Data.List.NonEmpty as NE
import System.Random
import Text.Show.Pretty (pPrint)


main :: IO ()
main = do
  g <- newStdGen
  let
    jobs = take 2000 $ (randomRs (Job 1 0 1 1, Job 8 1 1000 100)  g :: [Job])
    farmState = FarmState 0 [MachineSchedule (Machine 32 2 10000) i [] | i <- [1..100]]
--     endState = simulate firstFitScheduler jobs (H.singleton 0) farmState
    schedulers = [firstFitScheduler,
                  bestRAMFit, bestGPUFit, bestCoresFit, bestAllFit,
                  worstRAMFit, worstGPUFit, worstCoresFit, worstAllFit]
    results = map (\s -> currentTime $ simulate s jobs (H.singleton 0) farmState) schedulers

  pPrint results
