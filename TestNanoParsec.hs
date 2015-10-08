{- | this module is derived from the github source at

 "https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs"

 which is a part of the excellent tutorial
 Write You a Haskell ("http://dev.stephendiehl.com/fun/")
 authored by Stephen Diehl
-}

-- ----------------------------------------

module TestNanoParsec where

import Data.Char           (isSpace)
import Control.Monad       (when)
import Control.Applicative ((<$>), (<|>))

import Text.NanoParsec

-- ----------------------------------------

{- The grammar
              
 number  = [ "-" ] digit { digit }
 digit   = "0" | "1" | ... | "8" | "9"
 expr    = term { addop term }
 term    = factor { mulop factor }
 factor  = "(" expr ")" | number
 addop   = "+" | "-"
 mulop   = "*"

-}

-- ----------------------------------------
-- the abstract syntax
              
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Integer
  deriving Show

-- ----------------------------------------
-- the mini evaluator

eval :: Expr -> Integer
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

-- ----------------------------------------
-- the expr parser combinators

-- parse an integer literal
int :: Parser Expr
int = do xs <- token number
         return (Lit (read xs))

expr :: Parser Expr
expr = chainl1 term addop

-- term { addop term }

term :: Parser Expr
term = chainl1 factor mulop

-- factor { mulop factor }

factor :: Parser Expr
factor = int <|> do parens expr
--                    oneOf ['(']
--                    spaces
--                    e <- expr
--                    spaces
--                    oneOf [')']
--                    spaces
--                    return e

-- "(" expr ")" | number

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp s op = do reserved s
                  spaces
                  return op

addop :: Parser (Expr -> Expr -> Expr)
addop = do x <- oneOf ['+','-']
           spaces
           case x of
            '+' -> return Add
            '-' -> return Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = do x <- oneOf ['*']
           spaces
           return Mul
            
-- ----------------------------------------
-- the main prog

parse :: String -> Either String Expr
parse = runParser (spaces >> expr)

main :: IO ()
main = do
  putStr "calc> "
  ex <- getLine
  if ex == "."
    then return ()
    else do when (not (all isSpace ex)) $
              case parse ex of
              Left err -> putStrLn err
              Right e  -> print (eval e)
            main
            
-- ----------------------------------------

