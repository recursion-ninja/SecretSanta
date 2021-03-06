module Main where

import Control.Applicative     (liftA3)
import Data.Maybe
import File.Format.Participant.Internal
import Data.List.NonEmpty
import Prelude
import Safe
{-
import SecretSanta.Arrangement
import SecretSanta.Mailing
import SecretSanta.Participant
import SecretSanta.Solver
import SecretSanta.Types
-}
import System.Environment
import Text.Megaparsec


main :: IO ()
main = do
    stream <- getContents
    case (parse fileDefinition "STDIN" stream :: Either (ParseError Char Dec) (NonEmpty Participant)) of
      Left  error  -> putStrLn $ parseErrorPretty error
      Right result -> print result

{-
main :: IO ()
main = getArgs
   >>= fmap parseParameters . mapM readFile
   >>= maybe errorParametersMessage performLottery

performLottery :: Parameters -> IO ()
performLottery params = do 
  lot <- solveSecretSanta params
  case lot of
    Nothing -> putStrLn $ "Constraints unsatisfiable!" ++ show params
    Just x  -> notifyParticipants params x
            >> print x

parseParameters :: [String] -> Maybe Parameters
parseParameters xs
  = liftA3 Parameters participants' emailSetting' arrangements'
  where
    participants' = xs !? 0 >>= parseParticipants
    emailSetting' = xs !? 1 >>= parseEmailSettings
    arrangements' = whenNothingJust []
                  $ xs !? 2 >>= parseSecretSantaHistory

parseParticipants :: String -> Maybe [Participant]
parseParticipants = readMay

parseEmailSettings :: String -> Maybe EmailSettings
parseEmailSettings = readMay

parseSecretSantaHistory :: String -> Maybe [Arrangement]
parseSecretSantaHistory = readMay

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
-}
