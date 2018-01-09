module Main where

import Types
import Scheduler
import Simulator

import qualified Data.Heap as H
import System.Random
import Text.Show.Pretty (pPrint)


main :: IO ()
main = do
  g <- newStdGen
  let
    randJobs = take 1000 (randomRs (Job 1 0 1 1, Job 8 1 1000 100)  g :: [Job])
    farmState = FarmState { currentTime = 0
                          , machineSchedules = [MachineSchedule (ResourceBundle 32 4 10000) i [] | i <- [1..1]]
                          , stats = mempty
                          }

    endState = simulate firstFitScheduler randJobs (H.singleton 0) farmState
{-
    schedulers = [firstFitScheduler,
                  bestRAMFit, bestGPUFit, bestCoresFit, bestAllFit,
                  worstRAMFit, worstGPUFit, worstCoresFit, worstAllFit]
    results = map (\s -> currentTime $ simulate s randJobs (H.singleton 0) farmState) schedulers
-}

  pPrint endState
