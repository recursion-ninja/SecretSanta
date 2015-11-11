module Constraint.Strategy where

import Control.Applicative  (Applicative,pure)
import Control.Monad.Random (MonadRandom,fromList)
import Data.IntMap          (IntMap)
import Data.IntSet          (IntSet
import Data.Traversable     (sequenceA)

type ConstraintMap = IntMap IntSet
type Arrangement   = IntMap Int

class ConstraintStrategy s where
  feasibleArrangements :: s -> ConstraintMap a -> [Arrangement a]
  selectArrangement    :: (Applicative m, MonadRandom m) => s -> ConstraintMap a -> m (Maybe (Arrangement a))
  selectArrangement s xs -- Naive implemention, not performant
    | null options = pure Nothing
    | otherwise    = sequenceA . Just $ randomElement options
    where
      options       = feasibleArrangements s xs
      randomElement = fromList . flip zip (repeat 1)
