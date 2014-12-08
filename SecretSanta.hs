module Main where

import SecretSanta.Participant
import SecretSanta.Arrangement
import SecretSanta.ConstraintMap
import SecretSanta.Types
import Control.Applicative            ((<$>),(<*>),liftA2)
import Control.Monad                  ((>=>))
import Data.Map                hiding (filter,null)
import Data.Maybe
import Data.Traversable (sequenceA)
import Prelude                 hiding (lookup)
import Safe
import System.Environment

data Parameters
   = Parameters
   { participants :: ![Participant]
   , history      :: ![Arrangement]
   } deriving (Show)

main :: IO ()
main = getArgs
    >>= fmap parseParameters . mapM readFile
    >>= maybe errorMessage
          ( sequenceA
          . fmap (selectArrangement :: ConstraintMap -> IO Arrangement)
          . (constraintMap <$> participants <*> history)
        >=> print
          )


main2 :: IO ()
main2 = getArgs
   >>= mapM readFile --zipWithM ($) [readFile, readFile]
--   >>= print . (parseParameters >=> formulateArrangements)
--   >>= print . (parseParameters >=> uncurry constraints . \(Parameters x y) -> (x,y) )
--   >>= print . (parseParameters >=> uncurry arrangements . \(Parameters x y) -> (x,y) )
--   >>= print . fmap (concatMap (fmap snd . filter ((=="Sandy").fst) . assocs)) 
--     . (parseParameters >=> uncurry arrangements . \(Parameters x y) -> (x,y) )
--   >>= selectArrangement . parseParameters
   >>= print

parseParticipants :: String -> Maybe [Participant]
parseParticipants = readMay

parseSecretSantaHistory :: String -> Maybe [Arrangement]
parseSecretSantaHistory = readMay

parseParameters :: [String] -> Maybe Parameters
parseParameters xs
  = liftA2 Parameters participants' arrangements'
  where
    participants' = headMay xs >>= parseParticipants
    arrangements' = Just . fromMaybe [empty]
                  $ ( tailMay xs
                >>= headMay
                >>= parseSecretSantaHistory
                     )


errorMessage :: IO ()
errorMessage
    = getProgName
  >>= \x -> putStr
          $ unlines
          [ ""
          , "Failed to parse parameters!"
          , "Expected:"
          , "./" ++ x ++ " <Participants> <PreviousArangements> <EmailSettings>"
          , ""
          ]            
