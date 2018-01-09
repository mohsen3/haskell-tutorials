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


data ResourceBundle a = ResourceBundle { cores :: a
                                       , gpus  :: a
                                       , ram   :: a
                                       } deriving (Show, Eq, Bounded)

instance Functor ResourceBundle where
  fmap f (ResourceBundle c g r) = ResourceBundle (f c) (f g) (f r)

instance Num a => Monoid (ResourceBundle a) where
  mempty = ResourceBundle 0 0 0
  ResourceBundle c1 g1 r1 `mappend` ResourceBundle c2 g2 r2 = ResourceBundle (c1 + c2) (g1 + g2) (r1 + r2)

data Stats = Stats { time      :: Double
                   , resources :: ResourceBundle Double
                   } deriving Show

instance Monoid Stats where
  mempty = Stats 0 mempty

  Stats 0 _ `mappend` s2 = s2
  s1 `mappend` Stats 0 _ = s1
  Stats t1 (ResourceBundle c1 g1 r1) `mappend` Stats t2 (ResourceBundle c2 g2 r2) =
    let t = t1 + t2
        p1 = t1 / t
        p2 = t2 / t
    in
        Stats t (ResourceBundle (p1 * c1 + p2 * c2) (p1 * g1 + p2 * g2) (p1 * r1 + p2 * r2))

type Machine = ResourceBundle Word

type MachineId = Int
type Time = Word

data MachineSchedule = MachineSchedule { machine   :: Machine
                                       , machineId :: MachineId
                                       , jobs :: [(Time, Job)]
                                       } deriving Show

data FarmState = FarmState { currentTime      :: Time
                           , machineSchedules :: [MachineSchedule]
                           , stats            :: Stats
                           } deriving Show

type Scheduler = Time -> Job -> NonEmpty MachineSchedule -> MachineId

findCandidateMachines :: Time -> Job -> [MachineSchedule] -> [MachineSchedule]
findCandidateMachines currentTime job = filter (\ms -> job `fits` remainingResources currentTime ms)

fits :: Job -> Machine -> Bool
fits (Job jobCores jobGPUs jobRAM _) (ResourceBundle machineCores machineGPUs machineRAM) =
    machineCores >= jobCores && machineGPUs >= jobGPUs && machineRAM >= jobRAM

remainingResources :: Time -> MachineSchedule -> Machine
remainingResources currentTime MachineSchedule{..} = foldl subtractJob machine jobs
  where
    subtractJob :: Machine -> (Time, Job) -> Machine
    subtractJob machine@(ResourceBundle machineCores machineGPUs machineRAM) (startTime, Job jobCores jobGPUs jobRAM jobDuration)
      | startTime + jobDuration <= currentTime = machine -- job finished
      | otherwise = ResourceBundle (machineCores - jobCores) (machineGPUs - jobGPUs) (machineRAM - jobRAM)

addJobToSchedule :: Time -> [MachineSchedule] -> Job -> MachineId -> [MachineSchedule]
addJobToSchedule currentTime machineSchedules job mId = map updateMachine machineSchedules
  where
    updateMachine :: MachineSchedule -> MachineSchedule
    updateMachine machine | machineId machine == mId = machine{jobs=(currentTime, job):jobs machine}
                          | otherwise = machine
