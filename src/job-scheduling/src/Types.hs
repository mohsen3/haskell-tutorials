{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Data.List.NonEmpty (NonEmpty)
import System.Random

data Job =
     Job { cores    :: Word
         , gpus     :: Word
         , ram      :: Word
         , duration :: Word
         } deriving (Show, Eq, Bounded)

instance Random Job where
  random = randomR (minBound, maxBound)
  randomR (Job minCores minGPUs minRAM minDuration, Job maxCores maxGPUs maxRAM maxDuration) g =
    (Job{..}, g4)
    where
        (cores, g1) = randomR (minCores, maxCores) g
        (gpus, g2) = randomR (minGPUs, maxGPUs) g1
        (ram, g3) = randomR (minRAM, maxRAM) g2
        (duration, g4) = randomR (minDuration, maxDuration) g3


data Machine = Machine { cores :: Word
                       , gpus  :: Word
                       , ram   :: Word
                       } deriving (Show, Eq, Bounded)

type MachineId = Int
type Time = Word

data MachineSchedule = MachineSchedule { machine   :: Machine
                                       , machineId :: MachineId
                                       , jobs :: [(Time, Job)]
                                       } deriving Show

data FarmState = FarmState { currentTime      :: Time
                           , machineSchedules :: [MachineSchedule]
                           } deriving Show

type Scheduler = Time -> Job -> NonEmpty MachineSchedule -> MachineId

findCandidateMachines :: Time -> Job -> [MachineSchedule] -> [MachineSchedule]
findCandidateMachines currentTime job = filter (\ms -> job `fits` remainingResources currentTime ms)

fits :: Job -> Machine -> Bool
fits (Job jobCores jobGPUs jobRAM _) (Machine machineCores machineGPUs machineRAM) =
    machineCores >= jobCores && machineGPUs >= jobGPUs && machineRAM >= jobRAM

remainingResources :: Time -> MachineSchedule -> Machine
remainingResources currentTime MachineSchedule{..} = foldl subtractJob machine jobs
  where
    subtractJob :: Machine -> (Time, Job) -> Machine
    subtractJob machine@(Machine machineCores machineGPUs machineRAM) (startTime, Job jobCores jobGPUs jobRAM jobDuration)
      | startTime + jobDuration <= currentTime = machine -- job finished
      | otherwise = Machine (machineCores - jobCores) (machineGPUs - jobGPUs) (machineRAM - jobRAM)

addJobToSchedule :: Time -> [MachineSchedule] -> Job -> MachineId -> [MachineSchedule]
addJobToSchedule currentTime machineSchedules job mId = map updateMachine machineSchedules
  where
    updateMachine :: MachineSchedule -> MachineSchedule
    updateMachine machine | machineId machine == mId = machine{jobs=(currentTime, job):jobs machine}
                          | otherwise = machine
