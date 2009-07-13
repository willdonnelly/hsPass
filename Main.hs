module Main where

import Encryption
import Storage

import System ( getArgs )
import System.IO

import Data.Maybe

dbFile = "passwords.db"

showHelp = putStrLn "usage: hspass [display | create | auto]"

showPass :: [PassEntry] -> IO ()
showPass = putStr . unlines . map show

autoType :: [PassEntry] -> IO ()
autoType = undefined

editPass :: PassEntry -> IO PassEntry
editPass (PassEntry dName dUser dPass dDesc) = do
    name <- prompt dName "Name"
    user <- prompt dUser "User"
    pass <- prompt dPass "Pass"
    desc <- prompt dDesc "Desc"
    return $ PassEntry name user pass desc
  where prompt def msg = do
            putStr $ msg ++ "[" ++ def ++ "]: "
            hFlush stdout
            result <- getLine
            case result of
                 "" -> return def
                 nv -> return nv

defaultPass :: PassEntry
defaultPass = PassEntry "" "" "" ""

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
                         savePassDB key dbFile (newPass:db)
         "edit"    -> do (db, key) <- getDB
                         indexString <- prompt "Index: "
                         let index = read indexString
                         let (before, (here:after)) = splitAt index db
                         newEntry <- editPass here
                         savePassDB key dbFile (before ++ [newEntry] ++ after)
         "auto"    -> do (db, key) <- getDB
                         autoType db
         _         -> showHelp

getDB = do pass <- getPassword
           let key = stringToKey pass
           passDB <- loadPassDB key dbFile
           case passDB of Nothing -> getDB
                          Just db -> return (db, key)

prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

getPassword = prompt "Password: "
