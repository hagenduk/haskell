-- ----------------------------------------
--
-- simple operations on lists

module Data.List.SimpleOps
where

import Prelude hiding (splitAt)

-- ----------------------------------------

-- | The nub function removes duplicate elements from a list.
--
-- In particular, it keeps only the first occurrence of each element.
-- (The name nub means `essence'.)
--
-- Complexity class?

-- .1 nub with filter

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = undefined


-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = undefined


-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' = undefined


-- ----------------------------------------

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.

-- the spec
splitAt :: Int -> [a] -> ([a],[a])
splitAt i xs = (take i xs, drop i xs)

-- the impl
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' = undefined

-- ----------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.

-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate = undefined

-- 2. impl: with foldr
-- after chapter about folds
intercalate' :: [a] -> [[a]] -> [a]
intercalate' = undefined

-- ----------------------------------------

-- | The 'partition' function takes a predicate and a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--

-- the spec
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs
  = (filter p xs, filter (not . p) xs)

-- 1. impl: direct
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' = undefined

-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' = undefined

-- ----------------------------------------
--
-- | all prefixes of a list
      
-- 1. impl: direct

inits        :: [a] -> [[a]]
inits = undefined

-- 2. impl: with foldr
-- after chapter about folds

inits'        :: [a] -> [[a]]
inits' = undefined

-- ----------------------------------------

-- | concatenates 2 lists of strings
-- with a given char in between the elements
--
-- the following law must hold for split and join
--
--   join' c (split' c xs) == xs
--
--   join' c . split c == id
--

join' :: a -> [[a]] -> [a]
join' = undefined

split' :: Eq a => a -> [a] -> [[a]]
split' = undefined
    
-- ----------------------------------------
