module Sorting where

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y     = x : y : ys
    | otherwise = y : insert x ys

-- Insert sort algorithm
-- Take one element and insert into the sorted list
-- The list is sorted from the end, building up a sorted list.
-- Each element can therefore easily be inserted into the already sorted list
-- by simply iterating through the array as long as it is bigger than the current
-- element of the list
insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs

-- Insert sort using foldr
insertSortFold :: (Ord a) => [a] -> [a]
insertSortFold [] = []
insertSortFold list = foldr insert [] list

-- Merge sort algorithm
-- An example of a divide and conquer algorthm
-- First divide the list into two sublist. These are recursively sorted using merge sort
-- Merge the two sorted list together, which is done by building the list taking the smallest
-- of the first elements in the two lists, and repeating until both the lists are empty
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = split list
        n = (length list) `div` 2

        split :: [a] -> ([a], [a])
        split list = (take n list, drop n list)

        merge :: (Ord a) => [a] -> [a] -> [a]
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x < y     = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

-- Quick sort algorithm
-- Picks an element (pivot) in the list and sort all elements into two list,
-- one with all elements larger than the pivot and the other with
-- all elements less than or equal to the pivot.
-- The algorithm is then called recursivly these list, which are then concatenated
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

-- Bubble sort algorithm
-- Repeatily go through the list and swap pair next to each other such that they are ordered.
-- This means each iteration will insure that the last element is the largest element.
-- Bubbling elements up the list n times, where n is the length of the list, will ensure
-- that the list is sorted
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort list = iterate bubble list !! length list
    where
        bubble :: (Ord a) => [a] -> [a]
        bubble (x:y:rest)
            | x > y     = y : bubble (x:rest)
            | otherwise = x : bubble (y:rest)
        bubble xs = xs