module ADSB.Decoder.Position
    ( CPRPosition(..)
    , SurfaceMovement(..)
    , GroundTrack(..)
    , decodeCPRPosition
    , decodeSurfaceMovement
    , decodeGroundTrack
    , decodeAltitude
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

import ADSB.Decoder.Util

data CPRPosition = CPRPosition
    { cprLat :: Double
    , cprLon :: Double
    , cprOddEven :: Bool
    } deriving (Show)

data SurfaceMovement = SurfaceMovement
    { groundSpeed :: Maybe Double
    } deriving (Show)

data GroundTrack = GroundTrack
    { trackAngle :: Maybe Double
    , trackValid :: Bool
    } deriving (Show)

decodeCPRPosition :: BS.ByteString -> CPRPosition
decodeCPRPosition meField =
    let f = testBit (extractBits meField 21 1) 0  -- CPR Format (F) is bit 22
        lat = fromIntegral (extractBits meField 22 17) / 131072.0  -- LAT-CPR starts at bit 23
        lon = fromIntegral (extractBits meField 39 17) / 131072.0  -- LON-CPR starts at bit 40
    in CPRPosition lat lon f

decodeSurfaceMovement :: BS.ByteString -> SurfaceMovement
decodeSurfaceMovement meField =
    let movCode = extractBits meField 5 7
        speed = case movCode of
            0 -> Nothing  -- Speed not available
            1 -> Just 0   -- Stopped
            n | n >= 2 && n <= 8 -> Just (fromIntegral (n - 1) * 0.125)
            n | n >= 9 && n <= 12 -> Just (1 + fromIntegral (n - 9) * 0.25)
            n | n >= 13 && n <= 38 -> Just (2 + fromIntegral (n - 13) * 0.5)
            n | n >= 39 && n <= 93 -> Just (15 + fromIntegral (n - 39))
            n | n >= 94 && n <= 108 -> Just (70 + fromIntegral (n - 94) * 2)
            n | n >= 109 && n <= 123 -> Just (100 + fromIntegral (n - 109) * 5)
            124 -> Just 175
            _ -> Nothing  -- Reserved
    in SurfaceMovement { groundSpeed = speed }

decodeGroundTrack :: BS.ByteString -> GroundTrack
decodeGroundTrack meField =
    let statusBit = testBit (extractBits meField 12 1) 0
        trackCode = extractBits meField 13 7
        track = if statusBit
                then Just (fromIntegral trackCode * 360 / 128)
                else Nothing
    in GroundTrack { trackAngle = track, trackValid = statusBit }

decodeAltitude :: Word8 -> BS.ByteString -> Maybe Double
decodeAltitude tc meField
    | tc >= 9 && tc <= 18 = decodeBarometricAltitude meField
    | tc >= 20 && tc <= 22 = decodeGNSSHeight meField
    | otherwise = Nothing

decodeBarometricAltitude :: BS.ByteString -> Maybe Double
decodeBarometricAltitude meField = 
    let altCode = extractBits meField 8 12
    in if altCode == 0
        then Nothing  -- Altitude information not available
        else Just $ altitudeFromCode altCode
  where
    removeQBit :: Word32 -> Word32
    removeQBit code = (code `shiftR` 4 `shiftL` 3) .|. (code .&. 0x7)

    altitudeFromCode :: Word32 -> Double
    altitudeFromCode code =
        let qBit = testBit code 4
            codeWithoutQ = removeQBit code
        in if qBit
            then fromIntegral codeWithoutQ * 25 - 1000
            else fromIntegral (grayToBinary codeWithoutQ) * 100 - 1000

    grayToBinary :: Word32 -> Word32
    grayToBinary g = go g 0
      where
        go 0 acc = acc
        go x acc = go (x `shiftR` 1) (acc `xor` x)

decodeGNSSHeight :: BS.ByteString -> Maybe Double
decodeGNSSHeight meField =
    let heightCode = extractBits meField 8 12
    in Just $ fromIntegral heightCode * 25 - 1000
