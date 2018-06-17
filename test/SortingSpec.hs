module SortingSpec where

import Test.Hspec
import Data.List
import Sorting

spec :: Spec
spec = do
    describe "Insert sort" $ do
        it "Should return an empty lists" $
            insertSort ([]::[Int]) `shouldBe` ([]::[Int])
        it "Should return the sorted version of the list" $
            insertSort [3,2,1] `shouldBe` [1,2,3]
        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in insertSort list `shouldBe` sort list
        it "List with duplicate members" $
            let list = [7,2,6,5,4,8,9,5,4,3,5,8,7,5,4,3,21,7,5,679,656,4,1,1]
            in insertSort list `shouldBe` sort list

    describe "Insert sort using fold" $ do
        it "Should return an empty lists" $
            insertSortFold ([]::[Int]) `shouldBe` ([]::[Int])
        it "Should return the sorted version of the list" $
            insertSortFold [3,2,1] `shouldBe` [1,2,3]
        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in insertSortFold list `shouldBe` sort list
        it "List with duplicate members" $
            let list = [7,2,6,5,4,8,9,5,4,3,5,8,7,5,4,3,21,7,5,679,656,4,1,1]
            in insertSort list `shouldBe` sort list

    describe "Merge sort" $ do
        it "Should return an empty lists" $
            mergeSort ([]::[Int]) `shouldBe` ([]::[Int])
        it "Should return the sorted version of the list" $
            mergeSort [3,2,1] `shouldBe` [1,2,3]
        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in mergeSort list `shouldBe` sort list
        it "List with duplicate members" $
            let list = [7,2,6,5,4,8,9,5,4,3,5,8,7,5,4,3,21,7,5,679,656,4,1,1]
            in mergeSort list `shouldBe` sort list

    describe "Quick sort" $ do
        it "Should return an empty lists" $
            quickSort ([]::[Int]) `shouldBe` ([]::[Int])
        it "Should return the sorted version of the list" $
            quickSort [3,2,1] `shouldBe` [1,2,3]
        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in quickSort list `shouldBe` sort list
        it "List with duplicate members" $
            let list = [7,2,6,5,4,8,9,5,4,3,5,8,7,5,4,3,21,7,5,679,656,4,1,1]
            in quickSort list `shouldBe` sort list

    describe "Bubble sort" $ do
        it "Should return an empty lists" $
            bubbleSort ([]::[Int]) `shouldBe` ([]::[Int])
        it "Should return the sorted version of the list" $
            bubbleSort [3,2,1] `shouldBe` [1,2,3]
        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in bubbleSort list `shouldBe` sort list
        it "List with duplicate members" $
            let list = [7,2,6,5,4,8,9,5,4,3,5,8,7,5,4,3,21,7,5,679,656,4,1,1]
            in bubbleSort list `shouldBe` sort list