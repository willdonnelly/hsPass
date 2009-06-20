module Encryption
  ( stringToKey
  , CryptKey
  , encryptBS
  , decryptBS
  , encryptString
  , decryptString ) where

import Data.Word
import Data.Bits
import Data.List
import Data.LargeWord

import qualified Codec.Binary.UTF8.Light as UTF
import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Padding as PAD
import qualified Data.Digest.SHA256 as SHA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type CryptKey = Word256

-- The `stringToKey` function generates a 256-bit key from a
-- string of arbitrary length by taking the SHA256 digest of
-- the string and then doing some math to make the octets into
-- a single number.
stringToKey :: String -> Word256
stringToKey = fromIntegral . sum . zipWith (*) powers . octets
  where powers = [truncate (256 ** x) | x <- [0..31]]
        octets = map toInteger . SHA.hash . BS.unpack . UTF.encode

-- Break an array of Word128 into an equivalent array of Word8
break128 :: [Word128] -> [Word8]
break128 = concat . map break
  where break x = reverse . map (break' x) $ [0..15]
        break' x y = fromInteger $ (toInteger x) `shiftR` (y*8)

-- Combine each 16 elements of an array of Word8 so we get an
-- equivalent array of Word128
unbreak128 :: [Word8] -> [Word128]
unbreak128 = map join . split 16
  where split n = unfoldr (split' n)
          where split' n [] = Nothing
                split' n x = Just . splitAt n $ x
        join = sum . zipWith join' [0..15] . reverse
          where join' y x = fromInteger $ (toInteger x) `shiftL` (y*8)

-- Encrypt and decrypt a bytestring. Not a very efficient pair of
-- functions, but they work and seem decently fast.
encryptBS :: Word256 -> BS.ByteString -> BS.ByteString
encryptBS k = BS.pack . break128 . map (AES.encrypt k) . PAD.pkcs5 . BS.unpack

decryptBS :: Word256 -> BS.ByteString -> BS.ByteString
decryptBS k = BS.pack . PAD.unPkcs5 . map (AES.decrypt k) . unbreak128 . BS.unpack

-- Encrypt and decrypt a string. Since the result isn't guaranteed to be
-- valid unicode, we still use a bytestring for the encrypted text.
encryptString :: Word256 -> String -> BS.ByteString
encryptString k = encryptBS k . UTF.encode

decryptString :: Word256 -> BS.ByteString -> String
decryptString k = UTF.decode . decryptBS k
