{-# LANGUAGE TypeFamilies #-}

module File.Format.Participant.Internal where

import Control.Applicative
import Data.Char               (isSpace)
import Data.List.NonEmpty      (NonEmpty( (:|) ), some1)
import Data.Semigroup
import Text.Megaparsec
import Text.Megaparsec.Prim    (MonadParsec)


data  Participant
    = Participant
    { name  :: !ParticipantName
    , email :: !ParticipantEmail
    , tags  :: ![ParticipantTag]
    } deriving (Eq,Ord,Show) 


newtype ParticipantName  = PN (NonEmpty Char)
    deriving (Eq, Ord, Show)


newtype ParticipantEmail = PE (NonEmpty Char)
    deriving (Eq, Ord, Show)


newtype ParticipantTag   = PT (NonEmpty Char)
    deriving (Eq, Ord, Show)


fileDefinition :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Participant)
fileDefinition = some1 participantDefinition


participantDefinition :: (MonadParsec e s m, Token s ~ Char) => m Participant
participantDefinition =
    Participant
      <$> ( nameDefinition <* space)
      <*> (emailDefinition <* space)
      <*> ( tagsDefinition <* space)
  

nameDefinition :: (MonadParsec e s m, Token s ~ Char) => m ParticipantName
nameDefinition = space *> (PN <$> (anyChar `thisUntil` eol))


emailDefinition :: (MonadParsec e s m, Token s ~ Char) => m ParticipantEmail
emailDefinition = do
    _ <- space
    prefix <- nonSpaceCharacter `thisUntil` char '@'
    domain <- nonSpaceCharacter `thisUntil` char '.'
    suffix <- nonSpaceCharacter `thisUntil` eol
    pure . PE $ prefix <> ('@':|[]) <> domain <> ('.':|[]) <> suffix


 
tagsDefinition :: (MonadParsec e s m, Token s ~ Char) => m [ParticipantTag]
tagsDefinition = (tagDefinition `sepBy` inlineSpace) <* eol


tagDefinition :: (MonadParsec e s m, Token s ~ Char) => m ParticipantTag
tagDefinition = PT <$> some1 nonSpaceCharacter


-- | @thisUntil end@ consumes one or more characters until @end@ is matched, leaving @end@ in the stream
thisUntil :: MonadParsec e s m => m a -> m b -> m (NonEmpty a)
thisUntil c end = do
    _ <- notFollowedBy end
    (:|) <$> c <*> thisUntil' c end


-- | @thisUntil' end@ consumes zero or more characters until @end@ is matched, leaving @end@ in the stream
thisUntil' :: MonadParsec e s m => m a -> m b -> m [a]
thisUntil' c end = do
    ahead <- optional . try $ lookAhead end
    case ahead of
      Just _  -> pure []
      Nothing -> (:) <$> c <*> thisUntil' c end


nonSpaceCharacter :: (MonadParsec e s m, Token s ~ Char) => m Char
nonSpaceCharacter = satisfy (not . isSpace)
                        

-- | Consumes zero or more whitespace characters that are not newline characters
inlineSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
inlineSpace = skipMany inlineSpaceChar
  where
    inlineSpaceChar = satisfy $ \x -> isSpace x
                                   && '\n' /= x
                                   && '\r' /= x 



