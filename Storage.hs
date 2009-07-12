module Storage
  ( PassEntry (..)
  , PassDB
  , loadPassDB
  , savePassDB ) where

import Encryption

import qualified Data.Set as SET
import qualified Data.ByteString as BS
import qualified Directory as DIR

data PassEntry = PassEntry { description :: String
                           , username :: String
                           , password :: String
                           , comment :: String
                           } deriving (Read, Show, Eq, Ord)

type PassDB = SET.Set PassEntry

loadPassDB :: CryptKey -> String -> IO PassDB
loadPassDB k f = do present <- DIR.doesFileExist f
                    if present
                       then BS.readFile f >>= (return . loadPassDB' k)
                       else return SET.empty

loadPassDB' k bs
    | magic == magicString   = read passes
    | otherwise              = SET.empty
  where (magic, passes) = splitAt (length magicString) . decryptString k $ bs

savePassDB :: CryptKey -> String -> PassDB -> IO ()
savePassDB k f = BS.writeFile f . encryptString k . (magicString++) . show

magicString = "Magic Password File Header String (This Is Important)\n"
