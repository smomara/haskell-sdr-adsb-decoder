module ADSB.Detector
    ( isPreambleValid
    , findPreamble
    , detectModeS
    , detectADSBMessages
    ) where

import qualified Data.Vector.Storable as V
import Data.Word (Word16, Word8)
import Data.Bits
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.ByteString as BS
import ADSB.ErrorCorrection (fixErrors, crcCheck)

-- Constants
modesLongMsgBits :: Int
modesLongMsgBits = 112

modesShortMsgBits :: Int
modesShortMsgBits = 56

modesPreambleUs :: Int
modesPreambleUs = 8

-- Improved preamble validation
isPreambleValid :: V.Vector Word16 -> Bool
isPreambleValid m
    | V.length m < 16 = False
    | otherwise = and
        [ m V.! 0  > m V.! 1
        , m V.! 1  < m V.! 2
        , m V.! 2  > m V.! 3
        , m V.! 3  < m V.! 0
        , m V.! 4  < m V.! 0
        , m V.! 5  < m V.! 0
        , m V.! 6  < m V.! 0
        , m V.! 7  > m V.! 8
        , m V.! 8  < m V.! 9
        , m V.! 9  > m V.! 6
        , m V.! 4  < high
        , m V.! 5  < high
        , m V.! 11 < high
        , m V.! 12 < high
        , m V.! 13 < high
        , m V.! 14 < high
        ]
  where
    high = (m V.! 0 + m V.! 2 + m V.! 7 + m V.! 9) `div` 6

-- Find preamble in magnitude vector
findPreamble :: V.Vector Word16 -> Maybe Int
findPreamble magVector = 
    listToMaybe $ findIndices isPreambleValid $ slidingWindows 16 magVector

-- Helper function to create sliding windows
slidingWindows :: Int -> V.Vector Word16 -> [V.Vector Word16]
slidingWindows n vec
    | V.length vec < n = []
    | otherwise = V.slice 0 n vec : slidingWindows n (V.tail vec)

-- Helper function to find indices where the predicate is True
findIndices :: (V.Vector Word16 -> Bool) -> [V.Vector Word16] -> [Int]
findIndices p = map fst . filter (p . snd) . zip [0..]

-- Detect Mode S messages
detectModeS :: V.Vector Word16 -> V.Vector Word8 -> [BS.ByteString]
detectModeS magVector samples = catMaybes $ go 0
  where
    go index
        | index >= V.length magVector - modesLongMsgBits * 2 = []
        | otherwise = case findPreamble (V.drop index magVector) of
            Nothing -> []
            Just preambleIndex -> 
                let msgStart = index + preambleIndex + modesPreambleUs * 2
                    msg = decodeMessage (V.slice msgStart (modesLongMsgBits * 2) magVector) (V.slice msgStart (modesLongMsgBits * 2 `div` 8) samples)
                in msg : go (msgStart + modesLongMsgBits * 2)

-- Decode a single message
decodeMessage :: V.Vector Word16 -> V.Vector Word8 -> Maybe BS.ByteString
decodeMessage magVector samples
    | V.length magVector < modesLongMsgBits * 2 = Nothing
    | otherwise = 
        let bits = decodeBits magVector
            rawMsg = bitsToBytes bits
            fixedMsg = fixErrors rawMsg
        in if crcCheck fixedMsg
           then Just fixedMsg
           else Nothing

-- Decode bits from magnitude vector
decodeBits :: V.Vector Word16 -> V.Vector Bool
decodeBits magVector = V.generate modesLongMsgBits $ \i ->
    let index = i * 2
    in magVector V.! index > magVector V.! (index + 1)

-- Convert bits to bytes
bitsToBytes :: V.Vector Bool -> BS.ByteString
bitsToBytes bits = BS.pack $ V.toList $ V.generate (modesLongMsgBits `div` 8) $ \byteIndex ->
    let start = byteIndex * 8
        byteBits = V.slice start 8 bits
    in V.ifoldl' (\acc i b -> if b then setBit acc i else acc) 0 byteBits

-- Helper function to compute magnitude from I/Q samples
computeMagnitude :: V.Vector Word8 -> V.Vector Word16
computeMagnitude samples = V.generate (V.length samples `div` 2) $ \i ->
    let i' = i * 2
        re = fromIntegral (samples V.! i') - 127
        im = fromIntegral (samples V.! (i' + 1)) - 127
    in round $ sqrt $ fromIntegral (re * re + im * im :: Int)

-- Main detection function to be called from outside
detectADSBMessages :: V.Vector Word8 -> [BS.ByteString]
detectADSBMessages samples =
    let magnitudes = computeMagnitude samples
    in detectModeS magnitudes samples
