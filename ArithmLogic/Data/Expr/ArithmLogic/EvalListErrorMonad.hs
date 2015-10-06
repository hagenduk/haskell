{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.EvalListErrorMonad where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad
import Control.Monad.Except

import Data.List (partition)

import Data.Expr.ArithmLogic.Types
import Data.Expr.ArithmLogic.Constr
import Data.Pretty

-- ----------------------------------------
-- simple expression evaluation in monadic form
--
-- evaluates expressions without any free or bound variable
-- supports multiple results

-- ----------------------------------------

data Value
  = B Bool
  | I Integer
    deriving (Eq, Ord, Show)
             
instance Pretty Value where
  pretty (B b) = pretty b
  pretty (I i) = pretty i

isB :: Value -> Bool
isB (B _) = True
isB _     = False

isI :: Value -> Bool
isI (I _) = True
isI _     = False

-- ----------------------------------------

data Result a
  = R { resVal :: [a]       }
  | E { resErr :: EvalError }
  deriving (Show)

isR :: Result a -> Bool
isR (R _) = True
isR _     = False

isE :: Result a -> Bool
isE (E _) = True
isE _     = False

instance Functor Result where
  fmap = undefined

instance Applicative Result where
  pure = return
  (<*>) = ap

-- ----------------------------------------
-- there are different implementations of
-- the monad ops
--          
-- the pessimistic approach:
-- if a single error occurs, the whole computation fails
-- if no errors occur, there is at least one result,
-- the list will never be empty 
--
-- the optimistic approach:
-- if there's at least a single real result
-- possible errors are ignored
-- 
-- test case e11 in EvalSimple

instance Monad Result where
  return = undefined
  (>>=)  = undefined

instance Alternative Result where
  empty = mzero
  (<|>) = mplus

-- the @Mzero@ error case is catched by @mplus@
-- all other errors are propagated
  
instance MonadPlus Result where
  mzero = E Mzero
  mplus = undefined
  
instance MonadError EvalError Result where
  throwError = E
  catchError r@(R _) _ = r
  catchError   (E e) f = f e
  
instance (Pretty a) => Pretty (Result a) where
  pretty (R x) = pretty x
  pretty (E e) = "error: " ++ pretty e

-- ----------------------------------------
-- error handling
  
data EvalError
  = FreeVar String
  | NotImpl String
  | ValErr  String Value
  | MixVal [Value]
  | Div0
  | Mzero
  deriving (Show)

instance Pretty EvalError where
  pretty (FreeVar i)  = "free variable " ++ show i ++ " in expression"
  pretty (NotImpl n)  = n ++ " not implemented"
  pretty (ValErr e g) = e ++ " value expected, but got: "
                        ++ pretty g
  pretty (MixVal vs)  = "list of Integer or list of Bool expected, but got: "
                        ++ pretty vs
  pretty Div0         = "divide by zero"
  pretty Mzero        = "mzero"

boolExpected :: Value -> Result a
boolExpected = throwError . ValErr "Bool"

intExpected :: Value -> Result a
intExpected  = throwError . ValErr "Integer"

mixedValues :: [Value] -> Result a
mixedValues = throwError . MixVal

notImpl :: String -> Result a
notImpl = throwError . NotImpl

freeVar :: String -> Result a
freeVar = throwError . FreeVar

div0 :: Result a
div0  = throwError Div0

-- ----------------------------------------
--
-- the interpreter

eval :: Expr -> Result Value
eval (BLit b)          = return (B b)
eval (ILit i)          = return (I i)
eval (Var    x)        = freeVar x
eval (Unary  op e1)    = undefined
eval (Binary And e1 e2)
                       = undefined
eval (Binary Or  e1 e2)
                       = undefined
eval (Binary Impl e1 e2)
                       = undefined
eval (Binary Alt  e1 e2)   -- why this special case? Semantics of Alt?
                       = undefined
eval (Binary op e1 e2)
  | isStrict op        = undefined
eval (Binary op _ _)   = notImpl ("operator " ++ pretty op)
eval (Cond   c e1 e2)  = undefined
eval (Let _x _e1 _e2)  = notImpl "let expression"

evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        _     -> boolExpected r

-- type check for lists of values

checkMixed :: Result Value -> Result Value
checkMixed x@(R vs)
  | all isB vs || all isI vs
    = x
  | otherwise
    = mixedValues vs
      
-- ----------------------------------------
-- MF: Meaning function

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 UPlusMinus = uplmII

op1BB :: (Bool -> Bool) -> MF1
op1BB op (B b) = return $ B (op b)
op1BB _  v     = boolExpected v

op1II :: (Integer -> Integer) -> MF1
op1II op (I i) = return (I (op i))
op1II _  v     = intExpected v

op1BI :: (Bool -> Integer) -> MF1
op1BI op (B b) = return (I (op b))
op1BI _  v     = boolExpected v

uplmII :: MF1
uplmII v@(I x) = return v
                 `mplus`
                 return (I (0 - x))
uplmII v       = intExpected v

-- ----------------------------------------

isStrict :: Op2 -> Bool
isStrict op
  = op `elem`
    [ Xor, Equiv
    , Plus, Minus, Mult, Div, Mod
    , Eq, Neq, Gr, Ge, Ls, Le
    , PlusMinus, Alt
    ]
    
type MF2 = Value -> Value -> Result Value

mf2 :: Op2 -> MF2
mf2 And       = op2BBB (&&)
mf2 Or        = op2BBB (||)
mf2 Impl      = op2BBB (<=)
mf2 Xor       = op2BBB (/=)
mf2 Equiv     = op2BBB (==)
mf2 Plus      = op2III (+)
mf2 Minus     = op2III (-)
mf2 Mult      = op2III (*)
mf2 Div       = divIII div
mf2 Mod       = divIII mod
mf2 Eq        = op2IIB (==)
mf2 Neq       = op2IIB (/=)
mf2 Ge        = op2IIB (>=)
mf2 Gr        = op2IIB (>)
mf2 Le        = op2IIB (<=)
mf2 Ls        = op2IIB (<)
mf2 PlusMinus = plmIII
mf2 Alt       = altTTT

op2BBB :: (Bool -> Bool -> Bool) -> MF2
op2BBB op (B b1) (B b2) = return (B (b1 `op` b2))
op2BBB _  v1     v2
  | not (isB v1)        = boolExpected v1
  | otherwise           = boolExpected v2

op2III :: (Integer -> Integer -> Integer) -> MF2
op2III op (I i1) (I i2) = return (I (i1 `op` i2))
op2III _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

op2IIB :: (Integer -> Integer -> Bool) -> MF2
op2IIB op (I i1) (I i2) = return (B (i1 `op` i2))
op2IIB _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

divIII :: (Integer -> Integer -> Integer) -> MF2
divIII op (I x) (I y)
  | y == 0              = div0
  | otherwise           = return (I (x  `op` y))
divIII _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

plmIII :: MF2
plmIII (I x) (I y)      = return (I (x + y))
                          `mplus`
                          return (I (x - y))
plmIII v1    v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

altTTT :: MF2
altTTT v1    v2         = return v1
                          `mplus`
                          return v2

-- ----------------------------------------
