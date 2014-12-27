module Main where

import Control.Applicative                  (liftA2)
import Control.Monad                        ((>=>))
import Data.Map                      hiding (filter,null)
import Data.Maybe
import Prelude                       hiding (lookup)
import Safe
import SecretSanta.Arrangement
import SecretSanta.Participant
import SecretSanta.Solver
import SecretSanta.Types
import System.Environment

main :: IO ()
main = getArgs
    >>= fmap parseParameters . mapM readFile
    >>= maybe errorMessage
          ( solveSecretSanta
        >=> id
        >=> print
          )

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
          , "  ./" ++ x ++ " <Participants> <PreviousArangements> <EmailSettings>"
          , ""
          ]
