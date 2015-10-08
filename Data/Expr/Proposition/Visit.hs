-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr
  = V {vLit = Lit, vVar = Var, vUnary = Unary, vBinary= Binary}

visit :: Visitor r -> Expr -> r
visit v = visit'
     where
        visit' (Lit a) = (vLit v) a
        visit' (Var a) = (vVar v) a
        visit' (Unary f a) = (vUnary v) f (visit' a)
        visit' (Binary f a b) = (vBinary v) f (visit' a) (visit' b)

-- ----------------------------------------
