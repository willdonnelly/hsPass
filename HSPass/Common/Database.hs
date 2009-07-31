module HSPass.Common.Database ( withDatabase ) where

import HSPass.Core

getDB path getPass = do
    pass <- getPass
    let key = stringToKey pass
    passDB <- loadPassDB key path
    case passDB of
         Nothing -> getDB path getPass
         Just db -> return (db, key)

withDatabase passPrompt path dbAction = do
    (db, key) <- getDB path passPrompt
    newDB <- dbAction db
    case newDB of
         Nothing -> return ()
         Just db -> savePassDB key path db
