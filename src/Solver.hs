module Solver (dpll) where

import CNF (Atom (..), Clause, chooseLiteral, simplify)

-- DPLL Algorithm
dpll :: [Clause] -> [Atom] -> Maybe [Atom]
dpll clauses atoms
  | null clauses = Just atoms -- If there are no clauses left, we found a solution
  | any null clauses = Nothing -- If any clause is empty, it's UNSAT
  | otherwise =
      let literal = chooseLiteral clauses
          newClauses = simplify clauses literal
          newAtoms = updateAtoms atoms literal True
          resultWithLiteral = dpll newClauses newAtoms
       in case resultWithLiteral of
            Just _ -> resultWithLiteral -- Return the found solution
            Nothing -> dpll (simplify clauses (-literal)) (updateAtoms atoms literal False) -- Try the negated literal

-- Updates the atom valuation based on the chosen literal
updateAtoms :: [Atom] -> Int -> Bool -> [Atom]
updateAtoms atoms literal value =
  let index = abs literal - 1
   in take index atoms ++ [Atom (Just value)] ++ drop (index + 1) atoms
