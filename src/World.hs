module World ( LinkDirections (..)
             , roomCount
             , freshWorld
             , addRoom
             , accessibleRooms
             , linkRooms
             ) where

import Room (Room (..))
import qualified Data.Map.Lazy as M
import qualified Data.Set      as S

data World = World { nextRoomId :: Int
                   , rooms      :: M.Map Int Room
                   , roomLinks  :: M.Map Int (S.Set (Int, LinkDirections))
                   } deriving (Show, Eq)

data LinkDirections = North | East | West | South deriving (Show, Eq, Ord)

roomCount  = M.size . rooms

freshWorld = World { nextRoomId = 0
                   , rooms      = M.empty
                   , roomLinks  = M.empty
                   }

addRoom world room = (world { nextRoomId = successorId, rooms = newRooms }, roomWithId)
  where oldId       = nextRoomId world
        successorId = oldId + 1
        roomWithId  = room { roomId = oldId }
        newRooms    = M.insert oldId roomWithId (rooms world)

accessibleRooms world room = roomsWithDirections
  where links                        = roomLinks world
        idsAndDirections             = S.toList $ M.findWithDefault S.empty (roomId room) links
        roomsWithDirections          = map idToRoom idsAndDirections
        idToRoom (roomId, direction) = case M.lookup roomId (rooms world) of
                                         Nothing   -> error $ "Tried to access a room that doesn't exist in 'accessibleRooms'\r" ++ show world
                                         Just room -> (room, direction)

linkRooms world (r1, d1) (r2, d2) = world { roomLinks = finalRoomLinks }
  where rId1 = roomId r1
        rId2 = roomId r2
        updateRoomList roomId direction Nothing      = Just $ S.insert (roomId, direction) S.empty
        updateRoomList roomId direction (Just links) = Just $ S.insert (roomId, direction) links
        newRoomLinks   = M.alter (updateRoomList rId2 d1) rId1 (roomLinks world)
        finalRoomLinks = M.alter (updateRoomList rId1 d2) rId2 newRoomLinks
