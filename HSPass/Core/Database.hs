module HSPass.Core.Database
  ( loadPassDB
  , savePassDB
  , getDB
  , withDatabase
  ) where

import Numeric          ( showHex )
import System.Directory ( doesFileExist, canonicalizePath )
import Data.ByteString  ( writeFile, readFile )
import Prelude hiding   ( writeFile, readFile )

import HSPass.Core.Passwords
import HSPass.Core.Encryption

readDB :: CryptKey -> String -> Maybe [PassEntry]
readDB k db = if (hash == keyHash) then Just . read $ pdata else Nothing
  where (hash, pdata) = splitAt (length keyHash) db
        keyHash = showHex (toInteger k) ""

loadPassDB :: CryptKey -> String -> IO (Maybe [PassEntry])
loadPassDB k f = do
    filePath <- canonicalizePath f
    fileExists <- doesFileExist filePath
    if fileExists
       then readFile filePath >>= (return . readDB k . decryptString k)
       else return $ Just []

savePassDB :: CryptKey -> String -> [PassEntry] -> IO ()
savePassDB k f db = do
    filePath <- canonicalizePath f
    let stringDB = showHex (toInteger k) . show $ db
    writeFile filePath . encryptString k $ stringDB

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
