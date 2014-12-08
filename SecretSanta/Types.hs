module SecretSanta.Types
 (module SecretSanta.Participant
 ,module SecretSanta.Types
 ) where

import SecretSanta.Participant
import Data.Map (Map)
import Control.Monad.Random

type ConstraintMap     = Map Name [Name]
type Arrangement       = Map Name  Name
type CyclicArrangement = [Name]

class Constrainable a where
  toArrangement        :: a -> Arrangement
  feasibleArrangements :: ConstraintMap -> [a]
  selectArrangement    :: ConstraintMap -> IO a
  selectArrangement = let randomElement = fromList . flip zip (repeat 1)
                      in  randomElement . feasibleArrangements
