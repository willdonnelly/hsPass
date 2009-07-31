module HSPass.Actions.AutoType ( autoType ) where

import Data.List  ( isInfixOf )
import Data.Maybe ( fromMaybe )
import System.Automation.Type
import Graphics.UI.Dialog.Simple ( showPasswordDialog )

import HSPass.Common.Search
import HSPass.Core

sendPassword window passEntry = sendString window passString
  where passString = (user passEntry) ++ "\t" ++ (pass passEntry) ++ "\n"

autoType dbPath args config = do
    window <- getCurrentFocus
    withDatabase guiPrompt dbPath $ \db -> do
        title <- getWindowTitle window
        let match = searchSingle 0.08 title $ db
        case match of
             Nothing -> return ()
             Just mp -> sendPassword window mp
        freeWinRef window
        return Nothing

guiPrompt = do
    pass <- showPasswordDialog "Password" "Please enter the database password"
    return $ fromMaybe "" pass
