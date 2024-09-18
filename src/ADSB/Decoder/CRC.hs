module ADSB.Decoder.CRC
    ( computeCRC
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Char (digitToInt, intToDigit)
import Numeric (showHex, showIntAtBase)

hexToBin :: String -> String
hexToBin = concatMap ((\bin -> replicate (4 - length bin) '0' ++ bin) . flip (showIntAtBase 2 intToDigit) "" . digitToInt)

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

wrap :: String -> Int -> [String]
wrap [] _ = []
wrap xs n = take n xs : wrap (drop n xs) n

g :: [Int]
g = [0xFF, 0xFA, 0x04, 0x80]

crc :: String -> Int
crc msg = 
    let msgBin = hexToBin msg
        msgBinSplit = wrap msgBin 8
        mBytes = map binToInt msgBinSplit
        finalBytes = foldl processBytes mBytes [0..length mBytes - 4]
        [b1, b2, b3] = take 3 (reverse finalBytes)
    in (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

processBytes :: [Int] -> Int -> [Int]
processBytes bytes ibyte = 
    foldl (processOneBit ibyte) bytes [0..7]

processOneBit :: Int -> [Int] -> Int -> [Int]
processOneBit ibyte bytes ibit =
    let mask = 0x80 `shiftR` ibit
        bits = (bytes !! ibyte) .&. mask
    in if bits > 0
       then updateBytes bytes ibyte ibit
       else bytes

updateBytes :: [Int] -> Int -> Int -> [Int]
updateBytes bytes ibyte ibit =
    let updateByte i x = 
            x `xor` (0xFF .&. ((g !! (i-1) `shiftL` (8 - ibit)) .|. (g !! i `shiftR` ibit)))
    in  zipWith 
            (\i x -> if i == ibyte then x `xor` (g !! 0 `shiftR` ibit)
                     else if i > ibyte && i <= ibyte + 3 then updateByte (i - ibyte) x
                     else x) 
            [0..] 
            bytes

computeCRC :: BS.ByteString -> Word32
computeCRC rawBytes = 
    let hexString = concatMap (flip showHex "" . fromIntegral) $ BS.unpack rawBytes
    in fromIntegral $ crc hexString
