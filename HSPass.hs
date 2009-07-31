module HSPass ( hsPass, Config(..), defaultConfig ) where

import qualified Config.Dyre as Dyre

import Data.Maybe         ( fromMaybe, fromJust )
import System.Environment ( getArgs )

import HSPass.Config
import HSPass.Config.Default

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
