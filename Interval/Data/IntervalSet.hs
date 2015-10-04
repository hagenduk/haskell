module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
	| x1 <= x2 = y1+1 >= x2
	| otherwise = overlap (x2,y2) (x1,y1)


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = (y1 +1) < x2

                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y)
  = x>y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (a,b) (c,d)= ((min a c), (max b d))


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv [] = True
inv (x:[]) = True
inv (x1:x2:xs) = (not (emptyInterval x1)) && (not (overlap x1 x2)) && (less x1 x2) && (inv (x2:xs))


-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval (a,b) [] = singleInterval a b
insertInterval a (x:xs)
	| overlap a x = insertInterval (merge a x) xs 
	| less a x = [a, x] ++ xs
	| otherwise = (x : (insertInterval a xs))


fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList xs = fromIntervalList' xs []
	where
	fromIntervalList' [] a = a
	fromIntervalList' (x:xs) a = fromIntervalList' xs (insertInterval x a)


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union b [] = b
union [] a = a
union (x:xs) a = union xs (insertInterval x a) 


member :: Int -> IntervalSet -> Bool
member a [] = False
member a (x:xs) = (overlap' (a,a) x) || (member a xs)
	where
		overlap' (x1,y1) (x2,y2) = not (x1 < x2) || (x1 > y2)

         
fromList :: [Int] -> IntervalSet
fromList [] = []
fromList (x:xs) = insertInterval (x,x) (fromList xs) 


toList :: IntervalSet -> [Int]
toList [] = []
toList ((a,b):xs) = [a..b] ++ toList xs 


-- ----------------------------------------
