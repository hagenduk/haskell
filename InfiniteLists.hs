module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [[a] | a <- ['a'..'z']] ++ [[b] ++ show c | b <-['a'..'z'], c <- [1..9]]


-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = 0:1:zipWith (+) (tail fibs) fibs

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = primes'[2..]
      where
          primes' (x:xs) = x : primes' [y|y<-xs, (mod y x) /= 0] 

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = map (*2) [0..]
      is3 = [x | x <- [0..], (mod x 3) == 0]
      is5 = [5*i | i <- [0..]]

merge :: [Integer] -> [Integer] -> [Integer]
merge a [] = a
merge [] b = b
merge (x:xs) (a:b)
         | x == a = [x] ++ merge xs b
         | x < a = [x] ++ merge xs (a:b)
         | x > a = [a] ++ merge (x:xs) b
         
-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges [] = []
merges (x:xs) = merge x (merges xs)

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = foldl merge []  -- after chapter about folds

-- ----------------------------------------
