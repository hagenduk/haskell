module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
--substVars env e = undefined
substVars env (Lit a) = Lit a
substVars env (Var a) = snd $ (\[x] -> x) (filter (\x -> (fst x) == a) env)
substVars env (Unary op1 a) = Unary op1 (substVars env a)
substVars env (Binary op2 a b) = Binary op2 (substVars env a) (substVars env b)

freeVars :: Expr -> [Ident]
freeVars  = undefined

-- ----------------------------------------
