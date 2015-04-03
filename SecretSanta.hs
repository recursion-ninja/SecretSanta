module Main where

import Control.Applicative            (liftA3)
import Control.Monad                  ((>=>))
--import Data.Map                hiding (filter,null)
import Data.Maybe
import Prelude                 hiding (lookup)
import Safe
import SecretSanta.Arrangement
import SecretSanta.Mailing
import SecretSanta.Participant
import SecretSanta.Solver
import SecretSanta.Types
import System.Environment

main :: IO ()
main = getArgs
   >>= fmap parseParameters . mapM readFile
   >>= maybe errorParametersMessage
           (\x -> solveSecretSanta x
              >>= notifyParticipants x . fromJust
           )

parseParameters :: [String] -> Maybe Parameters
parseParameters xs
  = liftA3 Parameters participants' emailSetting' arrangements'
  where
    participants' = xs !? 0
                >>= parseParticipants
    emailSetting' = xs !? 1
                >>= parseEmailSettings
    arrangements' = whenNothingJust []
                  $ xs !? 2
                >>= parseSecretSantaHistory

parseParticipants :: String -> Maybe [Participant]
parseParticipants = readMay

parseSecretSantaHistory :: String -> Maybe [Arrangement]
parseSecretSantaHistory = readMay

parseEmailSettings :: String -> Maybe EmailSettings
parseEmailSettings = readMay

errorParametersMessage :: IO ()
errorParametersMessage
    = getProgName
  >>= \x -> putStr
          $ unlines
          [ ""
          , "Failed to parse parameters!"
          , "Expected:"
          , "  ./" ++ x ++ " <Participants> <EmailSettings> <PreviousArangements>"
          , ""
          ]

whenNothingJust :: a -> Maybe a -> Maybe a
whenNothingJust = (Just .) . fromMaybe

(!?) :: [a] -> Int -> Maybe a
(!?) xs n
  |  null xs
  || n <  0    = Nothing
  |  n == 0    = Just $ head xs
  |  otherwise = tail xs !? (n-1)
