module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
  = undefined


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = undefined

                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y)
  = undefined


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge = undefined


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv = undefined


-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval = undefined


fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList = undefined


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
union = undefined


member :: Int -> IntervalSet -> Bool
member = undefined

         
fromList :: [Int] -> IntervalSet
fromList = undefined


toList :: IntervalSet -> [Int]
toList = undefined


-- ----------------------------------------
