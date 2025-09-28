module TermDown.Time where

import Data.Time
import Data.Time.Format
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Text.Printf

-- Parse time specifications like "1h30m", "10", "14:30"
parseTimeSpec :: String -> Maybe (UTCTime -> UTCTime)
parseTimeSpec spec = 
  -- <|> tries expression from left to right until successful
  parseRelative spec <|> parseAbsolute spec <|> parseSimpleSeconds spec
  where
    parseSimpleSeconds s = do
      secs <- readMaybe s
      return $ addUTCTime (fromIntegral secs)
    
    parseRelative :: String -> Maybe (UTCTime -> UTCTime)
    parseRelative s = do
      secs <- parseTimedelta s
      return $ addUTCTime (fromIntegral secs)
    
    parseAbsolute :: String -> Maybe (UTCTime -> UTCTime)
    parseAbsolute s = do
      time <- parseTimeM True defaultTimeLocale "%H:%M" s
      return $ \now -> 
        let today = localDay $ utcToLocalTime (utc) now
        in localTimeToUTC (utc) $ LocalTime today time

parseTimedelta :: String -> Maybe Integer
parseTimedelta input = case words input of
  [] -> Nothing
  parts -> foldMap parseComponent parts
  where
    parseComponent s = case s of
      _ | Just n <- readMaybe (init s) -> case last s of
            's' -> Just n
            'm' -> Just (n * 60)
            'h' -> Just (n * 3600)
            'd' -> Just (n * 86400)
            _   -> Nothing
      _ -> readMaybe s

formatDuration :: Bool -> NominalDiffTime -> Text
formatDuration altFormat diffTime = 
  let totalSeconds = floor diffTime :: Integer
      hours = totalSeconds `div` 3600
      minutes = (totalSeconds `mod` 3600) `div` 60
      seconds = totalSeconds `mod` 60
  in if altFormat
     then pack $ printf "%02d:%02d:%02d" hours minutes seconds
     else pack $ printf "%dh%02dm%02ds" hours minutes seconds

formatTimeRemaining :: Bool -> UTCTime -> UTCTime -> Text
formatTimeRemaining altFormat now target =
  let diff = diffUTCTime target now
  in if diff <= 0 
     then pack "00:00:00"
     else formatDuration altFormat diff