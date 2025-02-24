module Parser where

import CNF (CNF (..), Clause)
import Data.Set qualified as Set
import Text.Parsec
  ( char,
    count,
    digit,
    eof,
    many,
    many1,
    manyTill,
    newline,
    noneOf,
    oneOf,
    skipMany,
    spaces,
    string,
  )
import Text.Parsec.String (Parser)

type Literal = Int

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

commentParser :: Parser ()
commentParser = do
  _ <- char 'c'
  skipMany (noneOf "\n")
  _ <- newline
  return ()

problemParser :: Parser (Int, Int)
problemParser = do
  _ <- char 'p'
  spaces
  _ <- string "cnf"
  spaces
  numVars <- read <$> many1 digit
  spaces
  numClauses <- read <$> many1 digit
  _ <- newline
  return (numVars, numClauses)

clauseParser :: Parser Clause
clauseParser = do
  literals <-
    manyTill
      ( do
          lit <- read <$> many1 (oneOf "-0123456789")
          spaces
          if lit == 0
            then
              fail "Unexpected zero before clause end"
            else
              return lit
      )
      (spaces >> char '0' >> spaces)
  return (Set.fromList literals)

cnfParser :: Parser CNF
cnfParser = do
  _ <- many commentParser
  (vars, clausesCount) <- problemParser
  clauses <- count clausesCount clauseParser
  eof
  return $ CNF vars clausesCount clauses
