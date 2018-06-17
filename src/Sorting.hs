module Sorting where

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y     = x : y : ys
    | otherwise = y : insert x ys

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs

insertSortFold :: (Ord a) => [a] -> [a]
insertSortFold [] = []
insertSortFold list = foldr insert [] list