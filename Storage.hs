module Storage
  ( PassEntry (..)
  , PassDB
  , loadPassDB
  , savePassDB ) where

import Encryption

import qualified Data.Set as SET
import qualified Data.ByteString as BS
import qualified Directory as DIR
import Numeric

data PassEntry = PassEntry { name :: String
                           , user :: String
                           , pass :: String
                           , desc :: String
                           } deriving (Read, Show, Eq, Ord)

type PassDB = SET.Set PassEntry

-- Add a new password to the database
addPass n u p d = SET.insert $ PassEntry n u p d

readDB :: CryptKey -> String -> Maybe PassDB
readDB k db = if (hash == keyHash) then Just . read $ pdata else Nothing
  where (hash, pdata) = splitAt (length keyHash) db
        keyHash = showHex (toInteger k) ""

loadPassDB :: CryptKey -> String -> IO (Maybe PassDB)
loadPassDB k f = do
    fileExists <- DIR.doesFileExist f
    if fileExists
       then BS.readFile f >>= (return . readDB k . decryptString k)
       else return . Just $ SET.empty

savePassDB :: CryptKey -> String -> PassDB -> IO ()
savePassDB k f = BS.writeFile f . encryptString k . showHex (toInteger k) . show
