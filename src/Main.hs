import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Parser
import System.Environment (getArgs)
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parse cnfParser "" content of
        Left err -> print err
        Right cnf -> print cnf
    _ -> putStrLn "Usage: ./sat-solver <filename>"
