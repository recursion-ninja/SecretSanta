module SecretSanta.Types
 (module SecretSanta.Participant
 ,module SecretSanta.Types
 ) where

import SecretSanta.Participant
import Data.Map (Map)
import Control.Monad.Random

type HamiltonianCycle = [Name]
type Arrangement      = Map Name  Name
type ConstraintMap    = Map Name [Name]

class Arrangementia a where
  feasibleArrangements :: ConstraintMap -> [a]
  selectArrangement    :: ConstraintMap -> IO a
  selectArrangement = let randomElement = fromList . flip zip (repeat 1)
                      in  randomElement . feasibleArrangements
