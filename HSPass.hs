module HSPass ( hsPass, Config(..), defaultConfig ) where

import qualified Config.Dyre as Dyre

import System.IO                      ( hFlush, stdout )
import Data.Maybe                     ( fromMaybe, fromJust )
import System.Environment             ( getArgs )
import System.Environment.XDG.BaseDir ( getUserDataFile )

import HSPass.Core
import HSPass.Common.Edit
import HSPass.Common.Passgen

import HSPass.Actions.Search
import HSPass.Actions.Reveal
import HSPass.Actions.Create
import HSPass.Actions.Modify
import HSPass.Actions.Delete
import HSPass.Actions.AutoType
import HSPass.Actions.KeePassX
import HSPass.Actions.JSON
import HSPass.Actions.Help

defaultConfig = Config
    { errorMsg    = Nothing
    , editPass    = editPassword
    , defaultPass = PassEntry "" "" "" "" ""
    , genPassword = generatePassword 32 $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    , passPath    = getUserDataFile "hsPass" "passwords"
    , plugins     = [ ("search", searchCommand)
                    , ("reveal", revealCommand)
                    , ("create", createCommand)
                    , ("modify", modifyCommand)
                    , ("delete", deleteCommand)
                    , ("autotype", autoType)
                    , ("keepassx-import", keepassxImport)
                    , ("keepassx-export", keepassxExport)
                    , ("import", importJSON)
                    , ("export", exportJSON)
                    , ("help",   showHelp)
                    ]
    , passPrompt = do putStr "Password: "
                      hFlush stdout
                      getLine
    }

confError cfg msg = cfg { errorMsg = Just msg }

realMain :: Config -> IO ()
realMain cfg@Config{passPath = passPath, plugins = plugins} = do
    arguments <- getArgs
    filePath  <- passPath
    let command = if null arguments then "" else head arguments
    let params  = if null arguments then [] else tail arguments
    let plugin = fromMaybe (fromJust $ "help" `lookup` plugins)
                           (command `lookup` plugins)
    plugin filePath params cfg

hsPass = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName = "hsPass"
    , Dyre.showError   = confError
    , Dyre.realMain    = realMain
    }
