module Parser where

import Data.List (intercalate)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Text.Parsec
  ( char,
    count,
    digit,
    eof,
    many,
    many1,
    newline,
    noneOf,
    oneOf,
    parse,
    skipMany,
    spaces,
    string,
    try,
  )
import Text.Parsec.String (Parser)

type Literal = Int

type Clause = [Literal]

data CNF = CNF {numVars :: Int, numClauses :: Int, clauses :: [Clause]}

toSubscript :: Int -> String
toSubscript n = concatMap toSub (show n)
  where
    toSub '0' = "₀"
    toSub '1' = "₁"
    toSub '2' = "₂"
    toSub '3' = "₃"
    toSub '4' = "₄"
    toSub '5' = "₅"
    toSub '6' = "₆"
    toSub '7' = "₇"
    toSub '8' = "₈"
    toSub '9' = "₉"
    toSub _ = ""

instance Show CNF where
  show :: CNF -> String
  show cnf = intercalate " ∧ " (map showClause (clauses cnf))
    where
      showClause clause = "(" ++ intercalate " ∨ " (map showLiteral clause) ++ ")"
      showLiteral lit
        | lit > 0 = "x" ++ toSubscript lit
        | otherwise = "¬x" ++ toSubscript (negate lit)

commentParser :: Parser ()
commentParser = do
  char 'c'
  skipMany (noneOf "\n")
  newline
  return ()

problemParser :: Parser (Int, Int)
problemParser = do
  char 'p'
  spaces
  string "cnf"
  spaces
  numVars <- read <$> many1 digit
  spaces
  numClauses <- read <$> many1 digit
  newline
  return (numVars, numClauses)

clauseParser :: Parser Clause
clauseParser = do
  literals <- many1 $ do
    lit <- read <$> many1 (oneOf "-123456789")
    spaces
    return lit
  char '0'
  spaces
  return literals

cnfParser :: Parser CNF
cnfParser = do
  many commentParser
  (vars, clausesCount) <- problemParser
  clauses <- count clausesCount clauseParser
  eof
  return $ CNF vars clausesCount clauses
