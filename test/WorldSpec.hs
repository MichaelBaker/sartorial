module WorldSpec where

import Test.Hspec            (describe, it, shouldBe)
import Test.Hspec.QuickCheck (property)
import Data.List             (foldl')

import Room  (freshRoom)
import World ( LinkDirections (..)
             , roomCount
             , freshWorld
             , addRoom
             , accessibleRooms
             , linkRooms
             )

spec = describe "World" $ do
  describe "roomCount" $ do
    it "is the total number of rooms in the world" $ do
      let populateWorld numRooms = foldl' (\w r -> fst $ addRoom w r) freshWorld $ replicate numRooms freshRoom
      roomCount (populateWorld 0)     `shouldBe` 0
      roomCount (populateWorld 100)   `shouldBe` 100
      roomCount (populateWorld 1000)  `shouldBe` 1000
      roomCount (populateWorld 10000) `shouldBe` 10000

  describe "linkRooms" $ do
    it "makes one room accessible from another room" $ do
      let (worldOne, roomOne) = addRoom freshWorld freshRoom
      let (worldTwo, roomTwo) = addRoom worldOne   freshRoom
      let world               = linkRooms worldTwo (roomOne, West) (roomTwo, East)
      accessibleRooms world roomOne `shouldBe` [(roomTwo, West)]
      accessibleRooms world roomTwo `shouldBe` [(roomOne, East)]

    it "only allows one way to get from one room to another in the same direction" $ do
      let (worldOne, roomOne) = addRoom freshWorld freshRoom
      let (worldTwo, roomTwo) = addRoom worldOne   freshRoom
      let worldThree          = linkRooms worldTwo   (roomOne, West) (roomTwo, East)
      let world               = linkRooms worldThree (roomOne, West) (roomTwo, East)
      accessibleRooms world roomOne `shouldBe` [(roomTwo, West)]
      accessibleRooms world roomTwo `shouldBe` [(roomOne, East)]
