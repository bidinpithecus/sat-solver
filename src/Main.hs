import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Parser
import System.Environment (getArgs)
import Text.Parsec

extractCnf :: Either ParseError Cnf -> Cnf
extractCnf clause = case clause of
  Left err -> error $ "Parsing error: " ++ show err
  Right arr -> arr

main :: IO ()
main = do
  content <- readFile "input/a.cnf"
  let lines = lines content
  let clauses = map (extractCnf . parseCnf) lines

  print $ clauses
