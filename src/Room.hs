module Room where

data Room = Room { name        :: String
                 , description :: String
                 }

freshRoom = Room { name        = "A Room"
                 , description = "An empty room"
                 }
