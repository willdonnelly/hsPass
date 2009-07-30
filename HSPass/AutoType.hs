module HSPass.AutoType ( autoType ) where

import System.Automation.Type
import Data.List
import HSPass.Passwords
import Control.Concurrent ( threadDelay )

sendPassword window passEntry = sendString window passString
  where passString = (user passEntry) ++ "\t" ++ (pass passEntry) ++ "\n"

autoType db args _ = do window <- getCurrentFocus
                        title <- getWindowTitle window
                        let matching = filter ((`isInfixOf` title) . titleText) db
                        if null matching
                           then return ()
                           else sendPassword window (head matching)
                        freeWinRef window
                        return Nothing
