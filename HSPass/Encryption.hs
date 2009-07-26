module HSPass.Encryption
  ( stringToKey, CryptKey
  , encryptString
  , decryptString ) where

import Data.Word
import Data.Bits
import Data.List
import Data.LargeWord

import qualified Codec.Binary.UTF8.String as UTF
import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Padding as PAD
import qualified Data.Digest.SHA256 as SHA
import qualified Data.ByteString as BS

type CryptKey = Word256

-- The `stringToKey` function generates a 256-bit key from a
-- string of arbitrary length by taking the SHA256 digest of
-- the string and then doing some math to make the octets into
-- a single number.
stringToKey :: String -> Word256
stringToKey = fromIntegral . sum . shift' 8 . octets
  where octets = map toInteger . SHA.hash . UTF.encode

-- Break an array of Word128 into an equivalent array of Word8
break128 :: [Word128] -> [Word8]
break128 = concat . map break'
  where break' = map fromInteger . shift' (-8) . replicate 16 . toInteger

-- Combine each 16 elements of an array of Word8 so we get an
-- equivalent array of Word128
unbreak128 :: [Word8] -> [Word128]
unbreak128 = map (fromInteger . sum . shift' 8 . map toInteger) . split 16
  where split  n   = unfoldr $ split' n
        split' n r = case r of [] -> Nothing; x -> Just $ splitAt n x

-- This helper applies either shiftR or shiftL
-- to a series of numbers, moving each successive number
-- one byte further than the last
shift'  :: (Bits a) => Int -> [a] -> [a]
shift' s = zipWith (flip shift) [0,s..]

-- Encrypt and decrypt a string. Since the result isn't guaranteed to be
-- valid unicode, we use a bytestring for the encrypted text and don't try
-- to look at it.
encryptString :: Word256 -> String -> BS.ByteString
encryptString k = BS.pack . encryptBlocks . UTF.encode
  where encryptBlocks = break128 . map (AES.encrypt k) . PAD.pkcs5

decryptString :: Word256 -> BS.ByteString -> String
decryptString k = UTF.decode . decryptBlocks . BS.unpack
  where decryptBlocks = PAD.unPkcs5 . map (AES.decrypt k) . unbreak128
