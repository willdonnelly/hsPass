module HSPass.Config where

import HSPass.Passwords

type PluginCommand = [PassEntry] -> [String] -> Config -> IO (Maybe [PassEntry])

data Config = Config
    { errorMsg    :: Maybe String
    , editPass    :: PassEntry -> IO PassEntry
    , defaultPass :: PassEntry
    , passPath    :: IO String
    , plugins     :: [(String, PluginCommand)]
    , passPrompt  :: IO String
    , typePrompt  :: IO String
    }
