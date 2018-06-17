module InsertSortSpec where

import Test.Hspec
import Data.List
import InsertSort

spec :: Spec
spec = do
    describe "Setting up tests" $ do
        it "Should return an empty lists" $
            insertSort ([]::[Int]) `shouldBe` ([]::[Int])

        it "Should return the sorted version of the list" $
            insertSort [3,2,1] `shouldBe` [1,2,3]

        it "Should return the sorted list" $
            let list = [4,2,3,1]
            in insertSort list `shouldBe` sort list