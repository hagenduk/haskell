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
nub (x : xs) = x : (filter (\y -> not (y==x)) (nub xs))


-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x : (nub' [y |y<-xs, x/=y])


-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' l = foldr (\x xs -> x : filter (/=x) xs) [] l


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
aplitAt' a [] = []
splitAt' a (x:xs)
			| a < 0 = error "nothing selected"
			| a == 0 = ([], (x:xs))
			| otherwise = (x : t, d)
			where
				(t,d) = splitAt' (a-1) xs
-- ----------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.

-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate a [] = []
intercalate _ (xs:[]) = xs
intercalate a (xs:xss) = xs ++ a ++ (intercalate a xss)

-- 2. impl: with foldr
-- after chapter about folds
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' _ (xs:[]) = xs
intercalate' a (xs:xss) = xs ++ foldr (\c d -> c ++ a ++ d) [] xss

-- TODO reihenfolge muss noch überarbeitet werden

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
partition' _ [] = ([],[])
partition' f (x:xs)
		| f x = (x : t, d)
		| otherwise = (t ,x : d)
		where
			(t,d) = partition f xs

-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' _ [] = ([],[])
partition'' f l = foldr (\x (a,b) -> (a ++ fst (sub f x), b ++ snd (sub f x))) ([],[]) l
			where
				sub f e
					| f e = ([e],[])
					| otherwise = ([], [e])
-- ----------------------------------------
--
-- | all prefixes of a list
      
-- 1. impl: direct

inits        :: [a] -> [[a]]
inits [] = []
inits x = [x] ++ inits (init x)  

-- 2. impl: with foldr
-- after chapter about folds

inits'        :: [a] -> [[a]]
inits' [] = []
inits' x = foldr (\x xs -> [x] : (map (x:) xs)) [] x


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
join' a = intercalate [a] 

split' :: Eq a => a -> [a] -> [[a]]
split' a [] = [[]]
split' a (x:xs)
		| x==a = [] : split' a xs
		| otherwise = [[x] ++ y] ++ ys
			where
				(y:ys) = split' a xs
    
-- ----------------------------------------
