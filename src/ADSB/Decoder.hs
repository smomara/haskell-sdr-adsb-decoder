module ADSB.Decoder
    ( ADSBMessage(..)
    , MessageData(..)
    , CPRPosition(..)
    , SurfaceMovement(..)
    , GroundTrack(..)
    , AirborneVelocityInfo(..)
    , VelocityType(..)
    , VerticalRateSource(..)
    , OperationStatus(..)
    , SubType(..)
    , CapacityClass(..)
    , OperationalMode(..)
    , ICAO
    , decodeMessage
    , extractICAOAddress
    , extractTypeCode
    , decodeCPRPosition
    , decodeAltitude
    , decodeAirborneVelocity
    , decodeOperationStatus
    , extractBits
    ) where

import qualified Data.Vector.Storable as V
import Data.Word
import Data.Bits
import Data.Char (chr)
import Data.Fixed (mod')

type ICAO = (Word8, Word8, Word8)

data ADSBMessage = ADSBMessage
    { msgraw :: V.Vector Word8
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

data AirborneVelocityInfo = AirborneVelocityInfo
    { velocityType :: VelocityType
    , speed :: Maybe Double
    , heading :: Maybe Double
    , verticalRate :: Maybe Double
    , verticalRateSource :: VerticalRateSource
    , gnssBaroDiff :: Maybe Double
    } deriving (Show)

data VelocityType = GroundSpeed | AirSpeed deriving (Show, Eq)
data VerticalRateSource = GNSS | Barometric deriving (Show)

data OperationStatus = OperationStatus
    { subType :: SubType
    , capacityClass :: CapacityClass
    , operationalMode :: OperationalMode
    , version :: Int
    , nicSupplement :: Bool
    , nacPosition :: Int
    , geometricVerticalAccuracy :: Maybe Int
    , sourceIntegrityLevel :: Int
    , barometricAltitudeIntegrityOrTrackAngle :: Bool
    , horizontalReferenceDirection :: Bool
    , silSupplement :: Bool
    } deriving (Show)

data SubType = AirborneStatus | SurfaceStatus | Reserved Int deriving (Show, Eq)

data CapacityClass = AirborneCapacity Word16 | SurfaceCapacity Word16 deriving (Show, Eq)

data OperationalMode = AirborneOperationalMode Word16 | SurfaceOperationalMode Word16 deriving (Show, Eq)

decodeMessage :: V.Vector Word8 -> Maybe ADSBMessage
decodeMessage rawBytes 
    | V.length rawBytes /= 14 = Nothing
    | otherwise = 
        let icao = extractICAOAddress rawBytes
            tc = extractTypeCode rawBytes
            msgData = decodeMessageData tc (V.slice 4 7 rawBytes)
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

extractDF :: V.Vector Word8 -> Word8
extractDF rawBytes = (V.head rawBytes `shiftR` 3) .&. 0x1F

extractICAOAddress :: V.Vector Word8 -> ICAO
extractICAOAddress rawBytes = (rawBytes V.! 1, rawBytes V.! 2, rawBytes V.! 3)

extractTypeCode :: V.Vector Word8 -> Word8
extractTypeCode rawBytes = (rawBytes V.! 4) `shiftR` 3

decodeMessageData :: Word8 -> V.Vector Word8 -> MessageData
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

decodeCallsign :: V.Vector Word8 -> String
decodeCallsign meField = 
    takeWhile (/= ' ') $ map decodeChar $ extractChars $ V.toList $ V.slice 1 6 meField
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

decodeOperationStatus :: V.Vector Word8 -> OperationStatus
decodeOperationStatus meField =
    let st = decodeSubType (fromIntegral $ extractBits meField 6 3)
        cc = decodeCapacityClass st (fromIntegral $ extractBits meField 9 16)
        om = decodeOperationalMode st (fromIntegral $ extractBits meField 25 16)
        ver = fromIntegral $ extractBits meField 41 3
        nics = testBit (extractBits meField 44 1) 0
        nacp = fromIntegral $ extractBits meField 45 4
        gva = if st == AirborneStatus
              then Just $ fromIntegral $ extractBits meField 49 2
              else Nothing
        sil = fromIntegral $ extractBits meField 51 2
        baiOrTa = testBit (extractBits meField 53 1) 0
        hrd = testBit (extractBits meField 54 1) 0
        sils = testBit (extractBits meField 55 1) 0
    in OperationStatus st cc om ver nics nacp gva sil baiOrTa hrd sils

decodeSubType :: Word8 -> SubType
decodeSubType 0 = AirborneStatus
decodeSubType 1 = SurfaceStatus
decodeSubType n = Reserved (fromIntegral n)

decodeCapacityClass :: SubType -> Word16 -> CapacityClass
decodeCapacityClass AirborneStatus cc = AirborneCapacity cc
decodeCapacityClass _ cc = SurfaceCapacity cc

decodeOperationalMode :: SubType -> Word16 -> OperationalMode
decodeOperationalMode AirborneStatus om = AirborneOperationalMode om
decodeOperationalMode _ om = SurfaceOperationalMode om

decodeSurfaceMovement :: V.Vector Word8 -> SurfaceMovement
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

decodeGroundTrack :: V.Vector Word8 -> GroundTrack
decodeGroundTrack meField =
    let statusBit = testBit (extractBits meField 12 1) 0
        trackCode = extractBits meField 13 7
        track = if statusBit
                then Just (fromIntegral trackCode * 360 / 128)
                else Nothing
    in GroundTrack { trackAngle = track, trackValid = statusBit }

decodeAirborneVelocity :: V.Vector Word8 -> AirborneVelocityInfo
decodeAirborneVelocity meField =
    let subType = fromIntegral $ extractBits meField 5 3 :: Word8
        verticalRateInfo = decodeVerticalRate meField
        gnssBaroDiff = decodeGNSSBaroDiff meField
    in case subType of
        1 -> decodeGroundSpeed 1 meField verticalRateInfo gnssBaroDiff
        2 -> decodeGroundSpeed 2 meField verticalRateInfo gnssBaroDiff
        3 -> decodeAirSpeed 3 meField verticalRateInfo gnssBaroDiff
        4 -> decodeAirSpeed 4 meField verticalRateInfo gnssBaroDiff
        _ -> AirborneVelocityInfo GroundSpeed Nothing Nothing Nothing GNSS Nothing

decodeGroundSpeed :: Word8 -> V.Vector Word8 -> (Maybe Double, VerticalRateSource) -> Maybe Double -> AirborneVelocityInfo
decodeGroundSpeed subType meField (vr, vrSource) gbd =
    let ewSign = testBit (extractBits meField 13 1) 0
        ewVel = extractBits meField 14 10
        nsSign = testBit (extractBits meField 24 1) 0
        nsVel = extractBits meField 25 10
        
        velocityMultiplier = if subType == 2 then 4 else 1
        
        vx = if ewVel == 0 then 0 else -1 * velocityMultiplier * (fromIntegral ewVel - 1) * (if ewSign then 1 else -1)
        vy = if nsVel == 0 then 0 else -1 * velocityMultiplier * (fromIntegral nsVel - 1) * (if nsSign then 1 else -1)
        
        speed = sqrt (vx * vx + vy * vy)
        heading = (atan2 vx vy * 180 / pi + 360) `mod'` 360
    in AirborneVelocityInfo GroundSpeed (Just speed) (Just heading) vr vrSource gbd

decodeAirSpeed :: Word8 -> V.Vector Word8 -> (Maybe Double, VerticalRateSource) -> Maybe Double -> AirborneVelocityInfo
decodeAirSpeed subType meField (vr, vrSource) gbd =
    let headingAvailable = testBit (extractBits meField 13 1) 0
        hdg = extractBits meField 14 10
        airspeedType = testBit (extractBits meField 24 1) 0  -- 0 for IAS, 1 for TAS
        as = extractBits meField 25 10
        
        velocityMultiplier = if subType == 4 then 4 else 1
        
        heading = if headingAvailable
                  then Just $ fromIntegral hdg * 360 / 1024
                  else Nothing
        speed = if as == 0 
                then Nothing 
                else Just $ velocityMultiplier * (fromIntegral as - 1)
    in AirborneVelocityInfo AirSpeed speed heading vr vrSource gbd

decodeVerticalRate :: V.Vector Word8 -> (Maybe Double, VerticalRateSource)
decodeVerticalRate meField =
    let vrSource = if testBit (extractBits meField 35 1) 0 then Barometric else GNSS
        vrSign = testBit (extractBits meField 36 1) 0
        vrValue = extractBits meField 37 9
        vr = if vrValue == 0
             then Nothing
             else Just $ (fromIntegral vrValue - 1) * 64 * (if vrSign then -1 else 1)
    in (vr, vrSource)

decodeGNSSBaroDiff :: V.Vector Word8 -> Maybe Double
decodeGNSSBaroDiff meField =
    let diffSign = testBit (extractBits meField 48 1) 0
        diffValue = extractBits meField 49 7
    in if diffValue == 0 || diffValue == 127
       then Nothing
       else Just $ fromIntegral diffValue * 25 * (if diffSign then -1 else 1)

decodeCPRPosition :: V.Vector Word8 -> CPRPosition
decodeCPRPosition meField =
    let f = testBit (extractBits meField 21 1) 0  -- CPR Format (F) is bit 22
        lat = fromIntegral (extractBits meField 22 17) / 131072.0  -- LAT-CPR starts at bit 23
        lon = fromIntegral (extractBits meField 39 17) / 131072.0  -- LON-CPR starts at bit 40
    in CPRPosition lat lon f

decodeAltitude :: Word8 -> V.Vector Word8 -> Maybe Double
decodeAltitude tc meField
    | tc >= 9 && tc <= 18 = decodeBarometricAltitude meField
    | tc >= 20 && tc <= 22 = decodeGNSSHeight meField
    | otherwise = Nothing

decodeBarometricAltitude :: V.Vector Word8 -> Maybe Double
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

decodeGNSSHeight :: V.Vector Word8 -> Maybe Double
decodeGNSSHeight meField =
    let heightCode = extractBits meField 8 12
    in Just $ fromIntegral heightCode * 25 - 1000

computeCRC :: V.Vector Word8 -> Word32
computeCRC _ = 0  -- Placeholder, actual CRC computation needed

extractBits :: V.Vector Word8 -> Int -> Int -> Word32
extractBits vec startBit numBits = 
    let startByte = startBit `div` 8
        endByte = (startBit + numBits - 1) `div` 8
        relevantBytes = V.slice startByte (endByte - startByte + 1) vec
        shiftRight = (8 - (startBit + numBits) `mod` 8) `mod` 8
        allBits = V.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0 relevantBytes
        alignedBits = allBits `shiftR` shiftRight
        mask = (1 `shiftL` numBits) - 1
    in alignedBits .&. mask
