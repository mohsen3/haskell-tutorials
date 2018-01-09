module Simulator where

import Types

import qualified Data.Heap as H
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>))

-- remainingResources :: Time -> MachineSchedule -> Machine
computeStats :: Time -> [MachineSchedule] -> ResourceBundle Double
computeStats time schedules = fmap fromIntegral $ mconcat $ map (remainingResources time) schedules

simulate :: Scheduler -> [Job] -> H.MinHeap Time -> FarmState -> FarmState
simulate scheduler [] finishTimes farmState@(FarmState currentTime machineSchedules stats) =
    -- all jobs already scheduled
    case H.view finishTimes of
      Nothing -> farmState -- simulation finished
      Just (t, newFinishTimes) ->
        let newStats = Stats (fromIntegral(t - currentTime)) (computeStats currentTime machineSchedules)
            newFarmState = FarmState t machineSchedules (stats <> newStats)
        in  simulate scheduler [] newFinishTimes newFarmState
simulate scheduler (job:jobs) finishTimes (FarmState currentTime machineSchedules stats) =
    case NE.nonEmpty $ findCandidateMachines currentTime job machineSchedules of
      Nothing -> case H.view finishTimes of -- no machine available, wait for the first job to finish
        Nothing -> error $ "Job too large? " ++ show job
        Just (t, newFinishTimes) ->
          let newStats = Stats (fromIntegral(t - currentTime)) (computeStats currentTime machineSchedules)
              newFarmState = FarmState t machineSchedules (stats <> newStats)
          in  simulate scheduler (job:jobs) newFinishTimes newFarmState
      Just candidateMachines -> -- use the scheduler
        let machineId = scheduler currentTime job candidateMachines
        in  simulate scheduler
                     jobs
                     (H.insert (currentTime + duration job) finishTimes)
                     (FarmState currentTime (addJobToSchedule currentTime machineSchedules job machineId) stats)

