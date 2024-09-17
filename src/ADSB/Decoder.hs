module ADSB.Decoder
    ( ICAO
    , ADSBMessage(..)
    , MessageData(..)
    , decodeMessage
    , extractICAOAddress
    , extractTypeCode
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Char (chr)

import ADSB.Decoder.Position
import ADSB.Decoder.Velocity
import ADSB.Decoder.OperationStatus
import ADSB.Decoder.Util

type ICAO = (Word8, Word8, Word8)

data ADSBMessage = ADSBMessage
    { msgraw :: BS.ByteString
    , msgbits :: Int
    , msgtype :: Word8
    , crcOk :: Bool
    , crc :: Word32
    , icaoAddr :: ICAO
    , typeCode :: Word8
    , messageData :: MessageData
    } deriving (Show)

data MessageData
    = AircraftIdentificationData { identCallsign :: String }
    | SurfacePositionData
        { surfacePosition :: CPRPosition
        , surfaceMovement :: SurfaceMovement
        , surfaceGroundTrack :: GroundTrack
        }
    | AirbornePositionData
        { airborneAltitude :: Maybe Double
        , airbornePosition :: CPRPosition 
        }
    | AirborneVelocityData AirborneVelocityInfo
    | AircraftStatusData { aircraftStatus :: String }
    | TargetStateAndStatusData { targetState :: String }
    | AircraftOperationStatusData OperationStatus
    | UnknownMessageData
    deriving (Show)

decodeMessage :: BS.ByteString -> Maybe ADSBMessage
decodeMessage rawBytes 
    | BS.length rawBytes /= 14 = Nothing
    | otherwise = 
        let icao = extractICAOAddress rawBytes
            tc = extractTypeCode rawBytes
            msgData = decodeMessageData tc (BS.drop 4 $ BS.take 11 rawBytes)
        in Just ADSBMessage
            { msgraw = rawBytes
            , msgbits = 112
            , msgtype = extractDF rawBytes
            , crcOk = True  -- Placeholder, actual CRC check needed
            , crc = computeCRC rawBytes
            , icaoAddr = icao
            , typeCode = tc
            , messageData = msgData
            }

extractDF :: BS.ByteString -> Word8
extractDF rawBytes = (BS.head rawBytes `shiftR` 3) .&. 0x1F

extractICAOAddress :: BS.ByteString -> ICAO
extractICAOAddress rawBytes = (BS.index rawBytes 1, BS.index rawBytes 2, BS.index rawBytes 3)

extractTypeCode :: BS.ByteString -> Word8
extractTypeCode rawBytes = (BS.index rawBytes 4) `shiftR` 3

decodeMessageData :: Word8 -> BS.ByteString -> MessageData
decodeMessageData tc meField
    | tc >= 1 && tc <= 4 = AircraftIdentificationData { identCallsign = decodeCallsign meField }
    | tc >= 5 && tc <= 8 =
        let cprPos = decodeCPRPosition meField
            movement = decodeSurfaceMovement meField
            track = decodeGroundTrack meField
        in SurfacePositionData
            { surfacePosition = cprPos
            , surfaceMovement = movement
            , surfaceGroundTrack = track
            }
    | (tc >= 9 && tc <= 18) || (tc >= 20 && tc <= 22) = 
        let alt = decodeAltitude tc meField
            cprPos = decodeCPRPosition meField
        in AirbornePositionData
            { airborneAltitude = alt
            , airbornePosition = cprPos
            }
    | tc == 19 = AirborneVelocityData (decodeAirborneVelocity meField)
    | tc == 28 = AircraftStatusData { aircraftStatus = "Unknown" }  -- Placeholder
    | tc == 29 = TargetStateAndStatusData { targetState = "Unknown" }  -- Placeholder
    | tc == 31 = AircraftOperationStatusData (decodeOperationStatus meField)
    | otherwise = UnknownMessageData

decodeCallsign :: BS.ByteString -> String
decodeCallsign meField = 
    takeWhile (/= ' ') $ map decodeChar $ extractChars $ BS.unpack $ BS.drop 1 $ BS.take 7 meField
  where
    extractChars :: [Word8] -> [Int]
    extractChars [] = []
    extractChars (b1:b2:bs) = 
        let char1 = fromIntegral (b1 `shiftR` 2)
            char2 = fromIntegral ((b1 .&. 0x03) `shiftL` 4 .|. (b2 `shiftR` 4))
            char3 = fromIntegral ((b2 .&. 0x0F) `shiftL` 2 .|. (head bs `shiftR` 6))
            char4 = fromIntegral (head bs .&. 0x3F)
        in char1 : char2 : char3 : char4 : extractChars (tail bs)
    extractChars [b] = [fromIntegral (b `shiftR` 2)]

    decodeChar :: Int -> Char
    decodeChar n
        | n >= 1 && n <= 26 = chr (n + 64)  -- A-Z
        | n >= 48 && n <= 57 = chr n        -- 0-9
        | n == 32 = ' '                     -- Space
        | otherwise = '#'                   -- Invalid character

computeCRC :: BS.ByteString -> Word32
computeCRC _ = 0  -- Placeholder, actual CRC computation needed
