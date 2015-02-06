module SecretSanta.Mailing
  ( notifyParticipants
  ) where

import SecretSanta.Arrangement
import SecretSanta.Participant
import Data.List
import Data.Maybe

-- | Notifies each participant of thier recipiant.
-- | Returns a Maybe list of failed notifications
-- | Nothing indicates a success.

notifyParticipants :: [Participant] -> Arrangement -> IO (Maybe [String])
notifyParticipants people = const $ return Nothing

-- | 

craftMessageBody :: [Participant] -> (Name,Name) -> String
craftMessageBody people (from,to)
  =
  unlines
  [ salutation from
  , message
  , directive
  , show to ++ (show . email) to' 
  , contactPreamble
  , contactTable people
  ]
  where
    -- Slightly dangerous, fromJust should never throw exceptions
    from' = fromJust $ findParticipant from people
    to'   = fromJust $ findParticipant to   people

findParticipant _    [] = Nothing
findParticipant needle (test:haystack)
  | name test == needle = Just test
  | otherwise = findParticipant needle haystack 

salutation addressee = "Hello " ++ addressee ++ ","
  
message = unwords
  [ "Merry Christmas! For this years Christmas,"
  , "rather then everyone buying presents for everyone else,"
  , "which typically boils down to a gift card exchange,"
  , "each \"adult\" family member will purchase gift(s) for one other adult"
  , "not from thier imediate nulcear family."
  , "Each person's secret santa was chosen randomly"
  , "and assured not to be part of their nuclear family."
  , "Additionally the grandparents will not get a grandchild"
  , "as thier secret santa recipient."
  ]

directive = "Your secret santa gift recipiant is: "

contactPreamble = unwords
  [ "Here is a list of all the Secret Santa participants,"
  , "thier associated nuclear family, and email address:"
  ]

contactTable :: [Participant] -> String
contactTable = unlines . fmap contactRow

contactRow :: Participant -> String
contactRow p = uncolumns
  [ name  p  
  , show $ family p
  , email p
  ]

doubleUnlines = intercalate "\n\n"
uncolumns     = intercalate "\t"
