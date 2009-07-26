module HSPass.Passwords ( PassEntry (..) ) where

data PassEntry = PassEntry { name :: String
                           , user :: String
                           , pass :: String
                           , desc :: String
                           } deriving (Read, Show, Eq, Ord)
