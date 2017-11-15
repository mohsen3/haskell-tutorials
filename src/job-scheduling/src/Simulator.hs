module Simulator where

import Types

import qualified Data.Heap as H
import qualified Data.List.NonEmpty as NE


simulate :: Scheduler -> [Job] -> H.MinHeap Time -> FarmState -> FarmState
simulate scheduler [] finishTimes farmState = -- all jobs already scheduled
    case H.view finishTimes of
      Nothing -> farmState -- simulation finished
      Just (t, newFinishTimes) -> simulate scheduler [] newFinishTimes farmState{ currentTime = t }
simulate scheduler (job:jobs) finishTimes (FarmState currentTime machineSchedules) =
    case NE.nonEmpty $ findCandidateMachines currentTime job machineSchedules of
      Nothing -> case H.view finishTimes of -- no machine available, wait for the first job to finish
        Nothing -> error $ "Job too large? " ++ show job
        Just (t, newFinishTimes) -> simulate scheduler (job:jobs) newFinishTimes (FarmState t machineSchedules)
      Just candidateMachines -> -- use the scheduler
        let machineId = scheduler currentTime job candidateMachines
        in  simulate scheduler
                     jobs
                     (H.insert (currentTime + duration job) finishTimes)
                     (FarmState currentTime (addJobToSchedule currentTime machineSchedules job machineId))

