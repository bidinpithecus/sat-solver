module CNF (CNF (..), Clause, Atom (..), initializeAtoms, displayResult, simplify, chooseLiteral) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

type Clause = Set Int

newtype Atom = Atom {value :: Maybe Bool} deriving (Show, Eq)

data CNF = CNF
  { numVars :: Int,
    numClauses :: Int,
    clauses :: [Clause]
  }
  deriving (Show)

-- Initializes a list of atoms with undefined values
initializeAtoms :: Int -> [Atom]
initializeAtoms n = replicate n (Atom Nothing)

-- Displays the result: either "SAT" or "UNSAT", followed by the atom values
displayResult :: Maybe [Atom] -> String
displayResult Nothing = "UNSAT"
displayResult (Just atoms) = "SAT\n" ++ unwords (zipWith showAtom [1 ..] atoms) ++ " 0"
  where
    showAtom :: Int -> Atom -> String
    showAtom index (Atom (Just True)) = show index
    showAtom index (Atom (Just False)) = show (-index)
    showAtom index (Atom Nothing) = "*" ++ show index

-- Simplifies clauses based on unit propagation
simplify :: [Clause] -> Int -> [Clause]
simplify clauses literal = filter (not . Set.member literal) (map (Set.delete (-literal)) clauses)

-- Chooses a literal based on MOM heuristic
chooseLiteral :: [Clause] -> Int
chooseLiteral clauses = head $ Set.toList $ minimumBy (comparing Set.size) clauses
