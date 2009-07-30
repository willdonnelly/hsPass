module HSPass.Edit ( editPassword ) where

import HSPass.Passwords
import System.IO

editPassword :: PassEntry -> IO PassEntry
editPassword (PassEntry dName dUser dPass dDesc dFind) = do
    name <- prompt dName "Name"
    user <- prompt dUser "User"
    pass <- prompt dPass "Pass"
    desc <- prompt dDesc "Desc"
    find <- prompt dFind "Find"
    return $ PassEntry name user pass desc find
  where prompt def msg = do
            putStr $ msg ++ " [" ++ def ++ "]: "
            hFlush stdout
            result <- getLine
            case result of
                 "" -> return def
                 nv -> return nv
