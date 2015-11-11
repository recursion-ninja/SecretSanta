module Constraint.Strategy.Simple
  ( SimpleStrategy
  ) where

import Constraint.Strategy
import Data.Traversable (sequenceA)

data SimpleStrategy = Simple

instance ConstraintStrategy SimpleStrategy where
  feasibleArrangements = feasibleNormalArrangements

feasibleNormalArrangements :: ConstraintMap a -> [Arrangement a]
feasibleNormalArrangements = filter validArrangement . sequenceA
  where
    validArrangement = nodupes . elems
    nodupes []     = True
    nodupes (y:ys) = notElem y ys
                  && nodupes ys
