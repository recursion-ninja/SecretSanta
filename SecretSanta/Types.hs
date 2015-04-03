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
   , mailing      :: !EmailSettings
   , history      :: ![Arrangement]
   } deriving (Show)

data EmailSettings
   = EmailSettings
   { alias    :: !String
   , username :: !UserName
   , password :: !Password
   , hostname :: !HostName
   , port     :: !PortNumber
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
    = [ ( EmailSettings parsedAlias
                        parsedUserName
                        parsedPassword
                        parsedHostName
               ( toEnum parsedPort )
        , t''''''''''
        )
      | ("EmailSettings", t          ) <- lex   str
      , ("Alias"        , t'         ) <- lex   t
      , ( parsedAlias   , t''        ) <- reads t'
      , ("UserName"     , t'''       ) <- lex   t''
      , ( parsedUserName, t''''      ) <- reads t'''
      , ("Password"     , t'''''     ) <- lex   t''''
      , ( parsedPassword, t''''''    ) <- reads t'''''
      , ("HostName"     , t'''''''   ) <- lex   t''''''
      , ( parsedHostName, t''''''''  ) <- reads t'''''''
      , ("PortNumber"   , t''''''''' ) <- lex   t''''''''
      , ( parsedPort    , t'''''''''') <- reads t'''''''''
      ]

