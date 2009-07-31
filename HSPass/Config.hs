module HSPass.Config where

import HSPass.Passwords

type PluginCommand = FilePath -> [String] -> Config -> IO ()

data Config = Config
    { errorMsg    :: Maybe String
    , editPass    :: PassEntry -> IO PassEntry
    , defaultPass :: PassEntry
    , passPath    :: IO String
    , plugins     :: [(String, PluginCommand)]
    , passPrompt  :: IO String
    }
