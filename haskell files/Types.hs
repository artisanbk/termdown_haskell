module TermDown.Types where

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Text (Text)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar)

data TimerMode =
    Countdown { targetTime :: UTCTime }
  | Stopwatch { startTime :: UTCTime, laps :: [NominalDiffTime] }
  | CurrentTime

data TimerState = TimerState
  { mode :: TimerMode
  , isPaused :: Bool
  , pauseStart :: Maybe UTCTime
  , totalPaused :: NominalDiffTime
  }

data InputCommand =
    Quit
  | PauseResume
  | Reset
  | AddTen
  | SubtractTen
  | ToggleEndTime
  | Lap
  deriving (Show, Eq)

data Config = Config
  { blinkOnFinish :: Bool
  , criticalSeconds :: Int
  , noBell :: Bool
  , showEndTime :: Bool
  , timeFormat :: Text
  , altFormat :: Bool
  }

data AppState = AppState
  { timerState :: TVar TimerState
  , config :: Config
  , inputThread :: Maybe ThreadId
  , appRunning :: TVar Bool
  }