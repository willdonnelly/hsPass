module HSPass.Core.Passwords ( PassEntry (..) ) where

data PassEntry = PassEntry { name :: String
                           , user :: String
                           , pass :: String
                           , desc :: String
                           , titleText :: String
                           } deriving (Read, Show, Eq, Ord)
