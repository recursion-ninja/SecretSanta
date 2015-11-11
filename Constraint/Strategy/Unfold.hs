{-# LANGUAGE ExistentialQuantification #-}
module Constraint.Strategy.Unfold where

import Constraint.Strategy
import Control.Applicative  (Applicative,pure)
import Control.Monad.Random (MonadRandom,fromList)
import Data.Map             (Map)
import Data.Traversable     (sequenceA)

data UnfoldStrategy = forall c . ConstraintStrategy c => 
                      Unfold c (ConstraintMap a -> Maybe (ConstraintMap a))

instance ConstraintStrategy UnfoldStrategy where
  feasibleArrangements s@(Unfold c f) constraints
    | null result = maybe [] (feasibleArrangements s) $ f constraints
    | otherwise   = result
    where
      result = feasibleArrangements c 

  selectArrangement s@(Unfold c f) constraints = undefined
