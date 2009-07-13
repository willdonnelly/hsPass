module Storage
  ( PassEntry (..)
  , loadPassDB
  , savePassDB ) where

import Encryption

import qualified Data.ByteString as BS ( writeFile, readFile )
import qualified Directory as DIR      (doesFileExist)
import Numeric ( showHex )

data PassEntry = PassEntry { name :: String
                           , user :: String
                           , pass :: String
                           , desc :: String
                           } deriving (Read, Show, Eq, Ord)

readDB :: CryptKey -> String -> Maybe [PassEntry]
readDB k db = if (hash == keyHash) then Just . read $ pdata else Nothing
  where (hash, pdata) = splitAt (length keyHash) db
        keyHash = showHex (toInteger k) ""

loadPassDB :: CryptKey -> String -> IO (Maybe [PassEntry])
loadPassDB k f = do
    fileExists <- DIR.doesFileExist f
    if fileExists
       then BS.readFile f >>= (return . readDB k . decryptString k)
       else return $ Just []

savePassDB :: CryptKey -> String -> [PassEntry] -> IO ()
savePassDB k f = BS.writeFile f . encryptString k . showHex (toInteger k) . show
