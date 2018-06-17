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

    describe "Insert sort using fold" $ do
        it "Should return an empty lists" $
            insertSortFold ([]::[Int]) `shouldBe` ([]::[Int])

        it "Should return the sorted version of the list" $
            insertSortFold [3,2,1] `shouldBe` [1,2,3]

        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in insertSortFold list `shouldBe` sort list