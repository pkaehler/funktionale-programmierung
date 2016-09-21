module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
   | x1 > x2   = overlap (x2, y2) (x1, y1)
   | otherwise = y1 + 1 >= x2


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = y1 < x2


nullInterval :: Interval -> Bool
nullInterval (x, y)
  = x > y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge  (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv (x:y:[]) = not (overlap x y) && less x y && not (nullInterval x) && not (nullInterval y)
inv (x:y:xs) = not (overlap x y) && less x y && not (nullInterval x) && inv (y:xs)


-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval x [] = [x]
insertInterval x (n:xs)
  | overlap x n = merge x n : xs
  | less x n = x : n : xs
  | otherwise = n : insertInterval x xs


fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList  =foldr insertInterval []


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
