{-# LANGUAGE RecordWildCards #-}
module Main where

import Types

import Control.Applicative
import qualified Data.Heap as H
import qualified Data.List.NonEmpty as NE
import System.Random
import Text.Show.Pretty (pPrint)


simulate :: [Job] -> H.MinHeap Word -> FarmState -> FarmState
simulate [] finishTimes farmState = -- all jobs already scheduled
    case H.view finishTimes of
        Nothing -> farmState -- simulation finished
        Just (t, newFinishTimes) -> simulate [] newFinishTimes farmState{ currentTime = t }
simulate (job:jobs) finishTimes (FarmState currentTime machineSchedules) =
    case NE.nonEmpty $ findCandidateMachines currentTime job machineSchedules of
      Nothing -> case H.view finishTimes of -- no machine available, wait for the first job to finish
        Nothing -> error $ "Job too large? " ++ show job
        Just (t, newFinishTimes) -> simulate (job:jobs) newFinishTimes (FarmState t machineSchedules)
      Just candidateMachines -> -- use the scheduler
        let machineId = schedule job candidateMachines
        in  simulate jobs
                     (H.insert (currentTime + duration job) finishTimes)
                     (FarmState currentTime (addJobToShedule currentTime machineSchedules job machineId))


main :: IO ()
main = do
  g <- newStdGen
  let
    jobs = take 20 $ (randomRs (Job 1 1 1 1, Job 8 2 1000 10)  g :: [Job])
    farmState = FarmState 0 [MachineSchedule (Machine 32 4 10000) 111 [], MachineSchedule (Machine 32 4 10000) 222 []]
    endState = simulate jobs (H.singleton 0) farmState

  pPrint endState
