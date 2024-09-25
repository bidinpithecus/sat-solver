module Parser where

import System.Environment (getArgs)
import Text.Parsec
  ( char,
    count,
    digit,
    many,
    many1,
    newline,
    noneOf,
    oneOf,
    parse,
    skipMany,
    spaces,
    string,
  )
import Text.Parsec.String (Parser)

type Literal = Int

type Clause = [Literal]

data CNF = CNF {numVars :: Int, numClauses :: Int, clauses :: [Clause]}
  deriving (Show)

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
    lit <- read <$> many1 (oneOf "-0123456789")
    spaces
    return lit
  let clause = takeWhile (/= 0) literals
  return clause

cnfParser :: Parser CNF
cnfParser = do
  many commentParser
  (vars, clausesCount) <- problemParser
  cls <- count clausesCount clauseParser
  return $ CNF vars clausesCount cls
