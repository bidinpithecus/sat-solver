module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Example of a CNF file format
-- c Comment line
-- p cnf 5 3
-- 1 -5 -4 0
-- -1 5 3 4 0
-- -3 -4 0

data Cnf
  = Problem Int Int
  | Clause [Int]
  deriving (Show)

cnfParser :: Parser [Cnf]
cnfParser = many (try problemParser <|> try clauseParser <|> try commentParser)

problemParser :: Parser Cnf
problemParser = do
  _ <- string "p cnf "
  numVariables <- int
  numClauses <- int
  return $ Problem numVariables numClauses

clauseParser :: Parser Cnf
clauseParser = do
  clause <- many1 (int <* optional space)
  _ <- char '0'
  return $ Clause clause

commentParser :: Parser Cnf
commentParser = do
  _ <- string "c"
  _ <- manyTill anyChar newline
  return $ Clause []

int :: Parser Int
int = read <$> (char '-' *> many1 digit <|> many1 digit)

parseCnf :: String -> Either ParseError [Cnf]
parseCnf = parse cnfParser ""
