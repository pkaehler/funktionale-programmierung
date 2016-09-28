module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------

fromList        :: [a] -> List a
fromList l      = (\xs -> xs ++ l)

toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = P.id

singleton       :: a -> List a
singleton       = fromList . (:[])

-- (:) for functional lists
cons            :: a -> List a -> List a
cons e l        = (e:) . l

-- dual to cons
snoc            :: List a -> a -> List a
snoc l e        = (++[e]) . l

-- (++) for functional lists
append          :: List a -> List a -> List a
append          = (.)

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat          = P.foldr append empty

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f           = (++) . P.map f . toList

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = P.foldr op n . toList

-- head, tail, null
head            :: List a -> a
head            = P.head . toList

tail            :: List a -> List a
tail l          = P.tail . l

null            :: List a -> Bool
null            = P.null . toList

reverse         :: List a -> List a
reverse l       = P.reverse . l

-- ----------------------------------------
