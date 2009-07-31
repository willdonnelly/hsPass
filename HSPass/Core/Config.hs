module HSPass.Core.Config where

import HSPass.Core.Passwords

type PluginCommand = FilePath -> [String] -> Config -> IO ()

data Config = Config
    { errorMsg    :: Maybe String
    , editPass    :: PassEntry -> IO PassEntry
    , defaultPass :: PassEntry
    , genPassword :: IO String
    , passPath    :: IO String
    , plugins     :: [(String, PluginCommand)]
    , passPrompt  :: IO String
    }
