module ADSB.ErrorCorrection
    ( fixErrors
    , crcCheck
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, xor)
import ADSB.Decoder.CRC (computeCRC)
import Data.Maybe (listToMaybe)

-- Constants
modesLongMsgBits :: Int
modesLongMsgBits = 112

-- Fix single bit errors
fixSingleBitErrors :: BS.ByteString -> BS.ByteString
fixSingleBitErrors msg = 
    let msgLen = BS.length msg
    in BS.pack $ map (fixBit msgLen) [0..msgLen-1]
  where
    fixBit msgLen byteIndex = 
        let byte = BS.index msg byteIndex
            fixedByte = listToMaybe $ filter (\b -> crcValid (replaceByte msg byteIndex b)) [byte `xor` bit | bit <- [1,2,4,8,16,32,64,128]]
        in maybe byte id fixedByte

-- Fix two bit errors
fixTwoBitErrors :: BS.ByteString -> BS.ByteString
fixTwoBitErrors msg =
    let bits = BS.length msg * 8
        result = listToMaybe [fixedMsg | 
                              j <- [0..bits-1],
                              i <- [j+1..bits-1],
                              let fixedMsg = flipBits msg j i,
                              crcValid fixedMsg]
    in maybe msg id result

-- Flip two bits in the message
flipBits :: BS.ByteString -> Int -> Int -> BS.ByteString
flipBits msg j i =
    let byte1 = j `div` 8
        bitmask1 = (1 :: Word8) `shiftL` (7 - (j `mod` 8))
        byte2 = i `div` 8
        bitmask2 = (1 :: Word8) `shiftL` (7 - (i `mod` 8))
        flipped1 = BS.pack $ zipWith (\idx b -> if idx == byte1 then b `xor` bitmask1 else b) [0..] (BS.unpack msg)
        flipped2 = BS.pack $ zipWith (\idx b -> if idx == byte2 then b `xor` bitmask2 else b) [0..] (BS.unpack flipped1)
    in flipped2

-- Replace a byte in the message
replaceByte :: BS.ByteString -> Int -> Word8 -> BS.ByteString
replaceByte msg index newByte = BS.take index msg `BS.append` BS.singleton newByte `BS.append` BS.drop (index + 1) msg

-- Check if CRC is valid
crcValid :: BS.ByteString -> Bool
crcValid msg = computeCRC msg == 0

-- Main error correction function
fixErrors :: BS.ByteString -> BS.ByteString
fixErrors msg 
    | crcValid msg = msg  -- If the message is already valid, return it unchanged
    | otherwise = 
        let singleBitFixed = fixSingleBitErrors msg
        in if crcValid singleBitFixed
           then singleBitFixed
           else let twoBitFixed = fixTwoBitErrors msg
                in if crcValid twoBitFixed
                   then twoBitFixed
                   else msg  -- If no valid correction found, return the original message

-- CRC check function
crcCheck :: BS.ByteString -> Bool
crcCheck = crcValid
