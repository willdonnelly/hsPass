module HSPass.Actions.AutoType ( autoType ) where

import Data.List  ( isInfixOf )
import Data.Maybe ( fromMaybe )
import System.Automation.Type
import Graphics.UI.Dialog.Simple ( showPasswordDialog )

import HSPass.Core
import HSPass.Common.Database

sendPassword window passEntry = sendString window passString
  where passString = (user passEntry) ++ "\t" ++ (pass passEntry) ++ "\n"

autoType dbPath args config = do
    window <- getCurrentFocus
    withDatabase guiPrompt dbPath $ \db -> do
        title <- getWindowTitle window
        let matching = filter ((`isInfixOf` title) . titleText) db
        if null matching
           then return ()
           else sendPassword window (head matching)
        freeWinRef window
        return Nothing

guiPrompt = do
    pass <- showPasswordDialog "Password" "Please enter the database password"
    return $ fromMaybe "" pass
