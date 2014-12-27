module SecretSanta.Types
 (module SecretSanta.Participant
 ,module SecretSanta.Types
 ) where

import SecretSanta.Participant
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Control.Monad.Random

type ConstraintMap     = Map Name [Name]
type Arrangement       = Map Name  Name
type CyclicArrangement = [Name]

data Parameters
   = Parameters
   { participants :: ![Participant]
   , history      :: ![Arrangement]
   } deriving (Show)

class Constrainable a where
  toArrangement        :: a -> Arrangement
  feasibleArrangements :: ConstraintMap -> [a]
  selectArrangement    :: ConstraintMap -> IO (Maybe a)
  selectArrangement xs
    | null options = return Nothing
    | otherwise    = sequenceA . Just $ randomElement options
    where 
      options       = feasibleArrangements xs
      randomElement = fromList . flip zip (repeat 1)
