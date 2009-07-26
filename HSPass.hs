module HSPass ( hsPass, Config(..), defaultConfig ) where

import qualified Config.Dyre as Dyre

import System.Environment ( getArgs, getProgName )
import Data.List          ( intercalate )

import HSPass.Config
import HSPass.Database
import HSPass.Encryption

realMain :: Config -> IO ()
realMain cfg@Config{passPath = passPath, plugins = plugins} = do
    arguments <- getArgs
    filePath  <- passPath
    let command = if null arguments then "help" else head arguments
    let params  = if null arguments then [] else tail arguments
    case command `lookup` plugins of
         Nothing -> showHelp params cfg
         Just fn -> do (db, key) <- getDB filePath cfg
                       newDB <- fn db params cfg
                       case newDB of
                            Nothing -> return ()
                            Just db -> savePassDB key filePath db

showHelp args config = do
    progName <- getProgName
    putStrLn . unwords $
      [ "usage:"
      , progName
      , "["
      , intercalate " | " . fst . unzip . plugins $ config
      , "]"
      ]

getDB path cfg@Config{passPrompt = getPass} = do
    pass <- getPass
    let key = stringToKey pass
    passDB <- loadPassDB key path
    case passDB of
         Nothing -> getDB path cfg
         Just db -> return (db, key)

hsPass = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName = "hsPass"
    , Dyre.showError   = confError
    , Dyre.realMain    = realMain
    }
