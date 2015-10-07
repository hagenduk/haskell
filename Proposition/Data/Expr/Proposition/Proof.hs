module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable n
	| n == 0 = []
	| n == 1 = [[True], [False]]
	| otherwise = (map (True:) (truthTable (n-1))) ++ (map (False:) (truthTable (n-1)))

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e
  = proof'' e tt
	where
		tt = truthTable (length (freeVars e))
		proof'' e [] = Nothing
		proof'' e (x:xs)
					| not(eval (substVars env e)) = Just env
					| otherwise = proof'' e xs
					where
						env = zip (freeVars e) (map (Lit) x)


proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------
