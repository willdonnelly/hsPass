module Main where

import System    ( getArgs )
import System.IO ( hFlush, stdout )

import Encryption
import Storage
import Edit ( editPass )
import Auto ( autoType )

passPath :: String
passPath = "passwords.db"

helpText :: String
helpText = "usage: hspass [display | create | auto]"

defaultPass :: PassEntry
defaultPass = PassEntry "" "" "" ""

showPass :: [PassEntry] -> IO ()
showPass = putStr . unlines . map show

main = do
    arguments <- getArgs
    if null arguments
       then main' ["help"]
       else main' arguments

main' args = do
    case head args of
         "display" -> do (db, key) <- getDB
                         showPass db
         "create"  -> do (db, key) <- getDB
                         newPass <- editPass defaultPass
                         savePassDB key passPath (newPass:db)
         "edit"    -> do (db, key) <- getDB
                         indexString <- prompt "Index: "
                         let index = read indexString
                         let (before, (here:after)) = splitAt index db
                         newEntry <- editPass here
                         savePassDB key passPath (before ++ [newEntry] ++ after)
         "auto"    -> do (db, key) <- getDB
                         autoType db
         _         -> putStrLn helpText

getDB = do pass <- prompt "Password: "
           let key = stringToKey pass
           passDB <- loadPassDB key passPath
           case passDB of Nothing -> getDB
                          Just db -> return (db, key)

prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine
