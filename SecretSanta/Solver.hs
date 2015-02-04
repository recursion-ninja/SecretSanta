module SecretSanta.Solver
  ( solveSecretSanta
  ) where


import Data.Maybe
import SecretSanta.ConstraintMap
import SecretSanta.CyclicArrangement
import SecretSanta.Types

-- | Select a uniformly random secret santa arrangement with maximal feaible constraints 
-- | Recursively relaxes the arrangement history constraints if no valid solution exists.
-- | Will return Nothing iff there is no valid arrangement regardless of the arrangment histories
solveSecretSanta :: Parameters -> IO (Maybe Arrangement)
solveSecretSanta (Parameters xs ys) = solveConstraints xs ys Nothing

-- | Recursively relaxes the history requirements,
-- | Returning the first feasible arrangment subject to maximal history constraints
solveConstraints :: [Participant] -> [Arrangement] -> Maybe Arrangement -> IO (Maybe Arrangement)
solveConstraints people archive prev
  | isJust prev  = return prev
  | null archive = result
  | otherwise    = result >>= solveConstraints people (tail archive)
  where
    constraints = constraintMap people archive
    result      = getArrangement constraints

-- | Maybe select a uniformly random arrangement
getArrangement :: ConstraintMap -> IO (Maybe Arrangement)
getArrangement = fmap (fmap toArrangement)
               . (selectArrangement :: ConstraintMap -> IO (Maybe CyclicArrangement))
