module SecretSanta.Mailing
  ( notifyParticipants
  ) where

import SecretSanta.Arrangement
import SecretSanta.Participant
import SecretSanta.Types
import Control.Applicative
import Data.Text         (pack)
import Data.Text.Lazy    (fromStrict)
import Data.List
import Data.Map          (assocs,mapKeys)
import Data.Maybe
import Network.Mail.Mime (Part())
import Network.Mail.SMTP

-- | Notifies each participant of thier recipiant.
-- | Returns a Maybe list of failed notifications
-- | Nothing indicates a success.

-- ERRORS HERE, almost correct
notifyParticipants :: Parameters -> Arrangement -> IO ()
notifyParticipants parameters
  = mapM_ (dispatchMail . makeMail) . assocs
  . fmap toParticipant . mapKeys toParticipant 
  where
    people         = participants parameters
    settings       = mailing parameters 
    toParticipant  = fromJust . findParticipant people
    dispatchMail   = sendMailWithLogin' <$> hostname <*> port <*> username <*> password $ settings
    fromAddress    = Address <$> Just . pack . alias <*> pack . username $ settings
    toAddress      = Address <$> Just . pack . (\p -> name p ++ " " ++ (show . family) p) <*> pack . email
    makeMail (x,y) = simpleMail
                       fromAddress
                       [toAddress x] [] []
                       (pack "Secret Santa")
                       [craftMessageBody people (name x, name y)]

-- | 

craftMessageBody :: [Participant] -> (Name,Name) -> Part
craftMessageBody people (from,to)
  = plainTextPart
  . fromStrict
  . pack
  $ doubleUnlines
  [ salutation from
  , message
  , directive
  , recipient
  , contactPreamble
  , contactTable people
  ]
  where
    -- Slightly dangerous, fromJust should never throw exceptions
--    from' = fromJust $ findParticipant people from
    to'   = fromJust $ findParticipant people to
    recipient = unwords [ show to, (show . email) to' ]

findParticipant :: [Participant] -> Name -> Maybe Participant
findParticipant [] _ = Nothing
findParticipant (test:haystack) needle
  | name test == needle = Just test
  | otherwise = findParticipant haystack needle

salutation :: [Char] -> [Char]
salutation addressee = "Hello " ++ addressee ++ ","
  
message :: String
message = unwords
  [ "Merry Christmas! For this years Christmas,"
  , "rather then everyone buying presents for everyone else,"
  , "which typically boils down to a gift card exchange,"
  , "each \"adult\" family member will purchase gift(s) for one other adult"
  , "not from thier immediate nuclear family."
  , "Each person's secret santa was chosen randomly"
  , "and assured not to be part of their nuclear family."
  , "Additionally the grandparents will not get a grandchild"
  , "as thier secret santa recipient."
  ]

directive :: String
directive = "Your secret santa gift recipiant is: "

contactPreamble :: String
contactPreamble = unwords
  [ "Here is a list of all the Secret Santa participants,"
  , "thier associated nuclear family, and email address:"
  ]

contactTable :: [Participant] -> String
contactTable = unlines . fmap contactRow

contactRow :: Participant -> String
contactRow p = uncolumns
  [ name  p  
  , email p
  ]

doubleUnlines, uncolumns :: [String] -> String
doubleUnlines = intercalate "\n\n"
uncolumns     = intercalate "\t"
