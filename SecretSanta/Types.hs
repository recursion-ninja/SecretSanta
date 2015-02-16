module SecretSanta.Types
 (module SecretSanta.Participant
 ,module SecretSanta.Types
 ) where

import SecretSanta.Participant
import Control.Monad.Random
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Network.Mail.SMTP.Types
import Network.Socket (HostName,PortNumber)

type ConstraintMap     = Map Name [Name]
type Arrangement       = Map Name  Name
type CyclicArrangement = [Name]

data Parameters
   = Parameters
   { participants :: ![Participant]
   , sendmail     :: !EmailSettings
   , history      :: ![Arrangement]
   } deriving (Show)

data EmailSettings
   = EmailSettings
   { hostname :: !HostName
   , port     :: !PortNumber
   , username :: !UserName
   , password :: !Password
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

instance Read EmailSettings where
  readsPrec _ str
    = [ ( EmailSettings parsedHostName
               ( toEnum parsedPort )
                        parsedUserName
                        parsedPassword
        , t''''''''
        )
      | ("EmailSettings", t        ) <- lex   str
      , ("HostName"     , t'       ) <- lex   t
      , ( parsedHostName, t''      ) <- reads t'
      , ("PortNumber"   , t'''     ) <- lex   t''
      , ( parsedPort    , t''''    ) <- reads t'''
      , ("UserName"     , t'''''   ) <- lex   t''''
      , ( parsedUserName, t''''''  ) <- reads t'''''
      , ("Password"     , t''''''' ) <- lex   t''''''
      , ( parsedPassword, t'''''''') <- reads t'''''''
      ]

