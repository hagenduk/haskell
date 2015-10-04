-- http://hackage.haskell.org/package/QuickCheck

module TestIntervalSet where

import           Data.IntervalSet
import           Test.QuickCheck

-- ----------------------------------------

-- invariant test: list of elems as input

prop_inv0 :: [Int] -> Bool
prop_inv0 = undefined


-- invariant test: list of pairs

prop_inv :: [Pair] -> Bool
prop_inv = undefined


-- all elements in set?
prop_elem :: [Pair] -> Bool
prop_elem = undefined


-- ----------------------------------------
--
-- auxiliary data type for Arbitrary instance
-- of Pairs

newtype Pair = P (Int, Int)
          deriving (Show)

instance Arbitrary Pair where
  arbitrary
    = do x <- arbitrary
         y <- arbitrary
         return (P (x `min` y, x `max` y))

fromPairs :: [Pair] -> IntervalSet
fromPairs = fromIntervalList . map (\ (P p) -> p)

-- ----------------------------------------

testArgs :: Args
testArgs
  = stdArgs { maxSize=500
            , maxSuccess=200
            }

t0, t1, t2 :: IO ()
t0 = quickCheck prop_inv0
t1 = quickCheck (verbose prop_inv)
t2 = quickCheckWith testArgs prop_elem

-- ----------------------------------------
