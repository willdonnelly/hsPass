module Main where

import Encryption
import Storage

import System.IO

import Data.Set as SET

dbFile = "passwords.db"

main = do (passDB, key) <- getDB dbFile
          newDB <- dbAction passDB
          savePassDB key dbFile newDB

getDB file = do pass <- getPassword
                let key = stringToKey pass
                passDB <- loadPassDB key file
                case passDB of
                     Nothing -> getDB file
                     Just db -> return (db, key)

prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

getPassword = prompt "Password: "

dbAction :: PassDB -> IO PassDB
dbAction passDB = do
    command <- prompt "Action: "
    case command of
         "print" -> do print passDB
                       return passDB
         "add"   -> do name <- prompt "Name: "
                       user <- prompt "User: "
                       pass <- prompt "Pass: "
                       desc <- prompt "Desc: "
                       let new = PassEntry name user pass desc
                       return $ SET.insert new passDB
         _       -> do putStrLn "Unrecognized command"
                       return passDB
