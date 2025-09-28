import System.Environment
import System.Exit
import Data.Time (UTCTime, NominalDiffTime)
import Data.Text (Text)

parse :: [String] -> IO String
parse [] = usage >> exit
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit

usage :: IO ()
usage = putStrLn "Usage: timedown [-vh]"

version :: IO ()
version = putStrLn "Timedown haskell v0.1"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)

tac :: String -> String
tac = unlines . reverse .lines

main :: IO()
main = do
  getArgs >>= parse >>= putStr
  return ()