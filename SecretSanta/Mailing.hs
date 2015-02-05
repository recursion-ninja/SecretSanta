module SecretSanta.Mailing
  ( notifyParticipants
  ) where


--| Notifies each participant of thier recipiant.
--| Returns either a Maybe list of failed notifications
--| Nothing indicates a success.

notifyParticipants :: [Participant] -> Arrangement -> IO (Maybe [String])
notifyParticipants = const $ return Nothing

--| 

-- craftMessageBody :: [Participant] -> (Name,Name) -> String
