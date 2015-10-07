{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin a Null = a
bin Null b = b
bin a b = Bin a b 

instance Functor Tree where
  fmap f Null = Null
  fmap f (Tip a) = Tip (f a)
  fmap f (Bin a b) = bin (fmap f a) (fmap f b) 

instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined
  
instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = Null
  mappend = bin

-- fold elements like in a list from right to left
instance Foldable Tree where
  --foldr _ e t = undefined
  foldr op e Null = e
  foldr op e (Tip x) = op x e
  foldr op e (Bin a b) = foldr op (foldr op e b) a

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null = e
    visit' (Tip a) = tf a
    visit' (Bin a b) = bf (visit' a) (visit' b)

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (const 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (const 1) (\x y -> (min x y) +1)
maxDepth = visitTree 0 (const 1) (\x y -> (max x y) +1)

-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a)
viewL Null = Nothing
viewL (Tip a) = Just(a, Null)
viewL (Bin a b) = Just(c , bin 	x b)
		where
		(Just(c,x)) = viewL a

viewR :: Tree a -> Maybe (Tree a, a)
viewR Null = Nothing
viewR (Tip a) = Just(Null, a)
viewR (Bin b a) = Just(bin x b, c)
		where
		(Just(x,c)) = viewR a

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr (:) []

-- | runs in O(n^2) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (\x -> [x]) (++)

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- weak balancing criterion
fromList :: [a] -> Tree a
fromList [] = Null
fromList (x:[]) =  Tip x
fromList (x:xs) = bin (Tip x) (fromList xs)  

-- strong balancing criterion
fromList' :: [a] -> Tree a
fromList' [] = Null	
fromList' (x:[]) =  Tip x
fromList' xs = bin (fromList' a) (fromList' b)
		where 
		(a,b) = splitAt ((length xs) `div` 2) xs 

-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
