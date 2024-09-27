import Data.Map qualified as Map
import Parser (CNF (..), Clause, Literal, cnfParser)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (</>))
import Text.Parsec (parse)

-- Simplifies the CNF by applying unit propagation and returns the simplified CNF and the valuation
simplifyCnf :: CNF -> (CNF, [Literal])
simplifyCnf (CNF nVars _ clauses) =
  let (simplifiedClauses, valuation) = simplify clauses []
   in (CNF nVars (length simplifiedClauses) simplifiedClauses, valuation)
  where
    -- Simplifies clauses and accumulates the valuation (truth assignments)
    simplify :: [Clause] -> [Literal] -> ([Clause], [Literal])
    simplify clauses' valuation =
      let unitLiterals = findUnitLiterals clauses'
          newClauses = propagateUnitLiterals unitLiterals clauses'
          newValuation = valuation ++ unitLiterals -- Accumulate unit literals in the valuation
       in if clauses' == newClauses
            then (clauses', newValuation)
            else simplify newClauses newValuation

    -- Finds all unit literals
    findUnitLiterals :: [Clause] -> [Literal]
    findUnitLiterals = concatMap (\clause -> if length clause == 1 then clause else [])

    -- Propagates unit literals: removes clauses satisfied by unit literals and simplifies clauses with the negation
    propagateUnitLiterals :: [Literal] -> [Clause] -> [Clause]
    propagateUnitLiterals unitLits clauses' =
      filter
        (not . any (`elem` unitLits)) -- Remove clauses containing unit literals
        (map (filter (`notElem` map negate unitLits)) clauses') -- Remove negations of unit literals in other clauses

-- DPLL solver that returns both satisfiability and the complete model (truth assignments)
dpll :: CNF -> Map.Map Int Bool -> (Bool, Map.Map Int Bool)
dpll cnf assignments =
  let (simplifiedCnf, valuation) = simplifyCnf cnf
      newAssignments = foldl (\acc lit -> Map.insert (abs lit) (lit > 0) acc) assignments valuation -- Update assignments with unit propagation
   in case clauses simplifiedCnf of
        [] -> (True, newAssignments) -- CNF is satisfied, return current assignments
        c | any null c -> (False, newAssignments) -- Unsatisfiable
        _ ->
          let l = chooseLiteral simplifiedCnf
           in case dpll (assignLiteral l True simplifiedCnf) (Map.insert (abs l) (l > 0) newAssignments) of
                (True, model) -> (True, model)
                (False, _) -> dpll (assignLiteral l False simplifiedCnf) (Map.insert (abs l) False newAssignments)

-- Function to choose the next literal to try
chooseLiteral :: CNF -> Literal
chooseLiteral (CNF _ _ clauses) = head (concat clauses)

-- Function to assign a literal and simplify the CNF accordingly
assignLiteral :: Literal -> Bool -> CNF -> CNF
assignLiteral lit value (CNF nVars _ clauses) =
  let newClauses =
        if value
          then filter (notElem lit) clauses -- Remove clauses where the literal is satisfied
          else map (filter (/= -lit)) clauses -- Remove the negation of the literal from remaining clauses
   in CNF nVars (length newClauses) newClauses

-- Completes the model by assigning False to any unassigned variables
completeModel :: Int -> Map.Map Int Bool -> Map.Map Int Bool
completeModel numVars model =
  foldl (\acc var -> Map.insertWith (const id) var False acc) model [1 .. numVars]

-- Solves the CNF formula and returns both the satisfiability and the complete model
solve :: CNF -> (Bool, Maybe (Map.Map Int Bool))
solve cnf =
  let (isSat, model) = dpll cnf Map.empty
   in (isSat, if isSat then Just (completeModel (numVars cnf) model) else Nothing)

-- Formats the model for satisfiable results
formatModel :: Maybe (Map.Map Int Bool) -> String
formatModel Nothing = ""
formatModel (Just model) =
  let assignments = Map.toList model
   in unwords [(if val then "" else "-") ++ show var | (var, val) <- assignments]

-- Writes the result to a .res file in the output directory
writeResult :: String -> Bool -> Maybe (Map.Map Int Bool) -> IO ()
writeResult filename isSat maybeModel = do
  let outputDir = "output"
  createDirectoryIfMissing True outputDir
  let baseName = takeBaseName filename
  let outputPath = outputDir </> (baseName ++ ".res")
  writeFile outputPath $
    if isSat
      then "SAT\n" ++ formatModel maybeModel
      else "UNSAT"

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parse cnfParser "" content of
        Left err -> print (filename ++ " contains the following error: " ++ show err)
        Right cnf -> do
          let (isSat, model) = solve cnf
          writeResult filename isSat model
    _ -> putStrLn "Usage: ./sat-solver <filename>"
