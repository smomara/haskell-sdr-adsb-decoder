module ADSB.Decoder.Util
    ( extractBits
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

extractBits :: BS.ByteString -> Int -> Int -> Word32
extractBits bs startBit numBits = 
    let startByte = startBit `div` 8
        bitOffset = startBit `mod` 8
        relevantBytes = BS.take (((bitOffset + numBits + 7) `div` 8)) $ BS.drop startByte bs
        value = BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0 relevantBytes
        shiftRight = (8 - (bitOffset + numBits) `mod` 8) `mod` 8
        mask = (1 `shiftL` numBits) - 1
    in (value `shiftR` shiftRight) .&. mask
