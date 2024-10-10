module Main where

import CNF (CNF (clauses, numVars), displayResult, initializeAtoms)
import Parser (cnfParser)
import Solver (dpll)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: <program> <cnf_file>"
    else do
      let fileName = head args
      fileContent <- readFile fileName
      case parse cnfParser "" fileContent of
        Left err -> print err
        Right cnf -> do
          let atoms = initializeAtoms (numVars cnf)
          let result = dpll (clauses cnf) atoms
          putStrLn (displayResult result)
