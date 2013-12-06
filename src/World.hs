module World ( roomCount
             , freshWorld
             , addRoom
             ) where

import Room (Room ())
import qualified Data.Map.Lazy as M

data World = World { nextRoomId :: Int
                   , rooms      :: M.Map Int Room
                   }

roomCount  = M.size . rooms

freshWorld = World { nextRoomId = 0
                   , rooms      = M.empty
                   }

addRoom world room = world { nextRoomId = successorId, rooms = newRooms }
  where successorId = nextRoomId world + 1
        newRooms    = M.insert (nextRoomId world) room (rooms world)
