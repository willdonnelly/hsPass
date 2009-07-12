module Storage
  ( PassEntry (..)
  , PassDB
  , loadPassDB
  , savePassDB ) where

import Encryption

import qualified Data.Set as SET
import qualified Data.ByteString as BS
import qualified Directory as DIR
import Data.Maybe
import Control.Monad

data PassEntry = PassEntry { name :: String
                           , user :: String
                           , pass :: String
                           , desc :: String
                           } deriving (Read, Show, Eq, Ord)

type PassDB = SET.Set PassEntry

-- Add a new password to the database
addPass n u p d = SET.insert $ PassEntry n u p d

showDB :: PassDB -> String
showDB db = unlines . (magicString:) . map show . SET.elems $ db

readDB :: String -> Maybe PassDB
readDB db = do
    magic <- listToMaybe . lines $ db
    guard (magic == magicString)
    return . foldl (flip SET.insert) SET.empty . map read . tail . lines $ db

loadPassDB :: CryptKey -> String -> IO (Maybe PassDB)
loadPassDB k f = do
    fileExists <- DIR.doesFileExist f
    if fileExists
       then BS.readFile f >>= (return . readDB . decryptString k)
       else return . Just $ SET.empty

savePassDB :: CryptKey -> String -> PassDB -> IO ()
savePassDB k f = BS.writeFile f . encryptString k . showDB

magicString = "Magic Password File Header String (This Is Important)"
