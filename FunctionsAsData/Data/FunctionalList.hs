module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------

fromList        :: [a] -> List a
fromList l      = undefined

toList          :: List a -> [a]
toList l        = undefined

empty           :: List a
empty           = undefined

singleton       :: a -> List a
singleton e     = undefined

-- (:) for functional lists
cons            :: a -> List a -> List a
cons e l        = undefined

-- dual to cons
snoc            :: List a -> a -> List a
snoc l e        = undefined

-- (++) for functional lists
append          :: List a -> List a -> List a
append l1 l2    = undefined

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat          = undefined

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f           = undefined

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = undefined

-- head, tail, null
head            :: List a -> a
head            = undefined

tail            :: List a -> List a
tail            = undefined

null            :: List a -> Bool
null            = undefined

reverse         :: List a -> List a
reverse         = undefined

-- ----------------------------------------
