module HSPass.Config.Default ( defaultConfig, confError ) where

import System.IO                      ( hFlush, stdout )
import System.Environment.XDG.BaseDir ( getUserDataFile )

import HSPass.Passwords
import HSPass.Config
import HSPass.Util
import HSPass.Edit

import HSPass.Actions.Search
import HSPass.Actions.Reveal
import HSPass.Actions.Create
import HSPass.Actions.Modify
import HSPass.Actions.Delete
import HSPass.Actions.AutoType
import HSPass.Actions.Help

defaultConfig = Config
    { errorMsg    = Nothing
    , editPass    = editPassword
    , defaultPass = PassEntry "" "" "" "" ""
    , passPath    = getUserDataFile "hsPass" "passwords"
    , plugins     = [ ("search", searchCommand)
                    , ("reveal", revealCommand)
                    , ("create", createCommand)
                    , ("modify", modifyCommand)
                    , ("delete", deleteCommand)
                    , ("autotype", autoType)
                    , ("help",   showHelp)
                    ]
    , passPrompt = do putStr "Password: "
                      hFlush stdout
                      getLine
    }

confError cfg msg = cfg { errorMsg = Just msg }
