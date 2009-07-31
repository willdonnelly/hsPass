module HSPass.Actions.Help ( showHelp ) where

import Data.List          ( intercalate )
import System.Environment ( getProgName )

import HSPass.Core

showHelp dbPath args config = do
    progName <- getProgName
    putStrLn . unwords $
      [ "usage:"
      , progName
      , "["
      , intercalate " | " . fst . unzip . plugins $ config
      , "]"
      ]
