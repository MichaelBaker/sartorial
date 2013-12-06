module WorldSpec where

import Test.Hspec            (describe, it, shouldBe)
import Test.Hspec.QuickCheck (property)
import World                 (roomCount, freshWorld, addRoom)
import Room                  (freshRoom)
import Data.List             (foldl')

spec = describe "World" $ do
  describe "roomCount" $ do
    it "is the number of rooms" $ do
      let populateWorld numRooms = foldl' addRoom freshWorld $ replicate numRooms freshRoom
      roomCount (populateWorld 0)     `shouldBe` 0
      roomCount (populateWorld 100)   `shouldBe` 100
      roomCount (populateWorld 1000)  `shouldBe` 1000
      roomCount (populateWorld 10000) `shouldBe` 10000
