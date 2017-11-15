{-# LANGUAGE DuplicateRecordFields #-}
module Scheduler where

import Types

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE


firstFitScheduler :: Scheduler
firstFitScheduler _ _ machines = machineId $ NE.head machines

genericSingleFieldScheduler :: (Machine -> Word)
                            -> ((MachineSchedule -> MachineSchedule -> Ordering) -> NE.NonEmpty MachineSchedule -> MachineSchedule)
                            -> Scheduler
genericSingleFieldScheduler fieldAccessor chooser currentTime _ machines =
    let machineRes schedule = fieldAccessor (remainingResources currentTime schedule)
    in  machineId $ chooser (comparing machineRes) machines

bestRAMFit, bestGPUFit, bestCoresFit :: Scheduler
bestRAMFit = genericSingleFieldScheduler ram minimumBy
bestGPUFit = genericSingleFieldScheduler gpus minimumBy
bestCoresFit = genericSingleFieldScheduler cores minimumBy

worstRAMFit, worstGPUFit, worstCoresFit :: Scheduler
worstRAMFit = genericSingleFieldScheduler ram maximumBy
worstGPUFit = genericSingleFieldScheduler gpus maximumBy
worstCoresFit = genericSingleFieldScheduler cores maximumBy

productAggregator :: Machine -> Word
productAggregator (Machine c g r) = c * g * r

bestAllFit, worstAllFit :: Scheduler
bestAllFit = genericSingleFieldScheduler productAggregator minimumBy
worstAllFit = genericSingleFieldScheduler productAggregator maximumBy
