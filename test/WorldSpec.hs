module WorldSpec where

import Test.Hspec            (describe, it, shouldBe)
import Test.Hspec.QuickCheck (property)
import World                 (add)

spec = describe "World" $ do
  describe "add" $ do
    it "adds two numbers" $ do
      add 1 2 `shouldBe` 3

    it "adds two numbers reliably" $ property $ \x y ->
      add x y == (x + y)
