module Main where

import Encryption
import Storage

import System.IO

import Data.Set as SET

main = do
    pass <- getPassword
    let key = stringToKey pass
    passDB <- loadPassDB key "passwords.db"
    newDB <- dbAction passDB
    savePassDB key "passwords.db" newDB

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
