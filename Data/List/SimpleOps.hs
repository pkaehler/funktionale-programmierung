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
nub (x : xs) = x : nub (filter (/=x) xs)


-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x: nub' ([n | n<-xs, (n/=x)])


-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' = foldr filter' []
      where
      filter' x xs = x : filter (/=x) xs

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
splitAt' _ [] = ([], [])
splitAt' n (x : xs)
  | n <= 0 = ([], (x:xs))
  | n > 0  = (x:xs', ys')
  where (xs', ys') = splitAt' (n - 1) xs

-- ----------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.

-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []        = []
intercalate _ (xs:[])   = xs
intercalate ys (xs:xss) = xs ++ ys ++ intercalate ys xss

-- 2. impl: with foldr
-- after chapter about folds
intercalate' :: [a] -> [[a]] -> [a]
intercalate' ys xss = foldr1 (\a b -> a ++ ys ++ b) xss

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
partition' _ [] = ([], [])
partition' p (x:xs)
  | p x = (x:xs', ys')
  | otherwise = (xs', x:ys')
  where (xs', ys') = partition' p xs

-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' p = foldr test ([], [])
  where test x (xs, ys)
          | p x = (x:xs, ys)
          | otherwise = (xs, x:ys)

-- ----------------------------------------
--
-- | all prefixes of a list

-- 1. impl: direct

inits        :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = []:map (x:) (inits xs)

-- 2. impl: with foldr
-- after chapter about folds

inits'        :: [a] -> [[a]]
inits' = foldr test []
  where test x xs
          | null xs = [[], [x]]
          | otherwise = []:map (x:) xs

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
join' c (xs:[])  = xs
join' c (xs:xss) = xs ++ [c] ++ join' c xss

-- | splits the input into sublists at delimiter
--   1. arg is the delimiter
--   the delimiter does not occur in elements of result list

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = [[]]
split' sep (x:xs)
  | sep == x  = []:split' sep xs
  | otherwise = (x:y):ys
  where (y:ys) = split' sep xs

-- ----------------------------------------
