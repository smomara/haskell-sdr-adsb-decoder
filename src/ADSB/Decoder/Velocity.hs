module ADSB.Decoder.Velocity
    ( AirborneVelocityInfo(..)
    , VelocityType(..)
    , VerticalRateSource(..)
    , decodeAirborneVelocity
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word (Word8)
import Data.Fixed (mod')

import ADSB.Decoder.Util

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

decodeAirborneVelocity :: BS.ByteString -> AirborneVelocityInfo
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

decodeGroundSpeed :: Word8 -> BS.ByteString -> (Maybe Double, VerticalRateSource) -> Maybe Double -> AirborneVelocityInfo
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

decodeAirSpeed :: Word8 -> BS.ByteString -> (Maybe Double, VerticalRateSource) -> Maybe Double -> AirborneVelocityInfo
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

decodeVerticalRate :: BS.ByteString -> (Maybe Double, VerticalRateSource)
decodeVerticalRate meField =
    let vrSource = if testBit (extractBits meField 35 1) 0 then Barometric else GNSS
        vrSign = testBit (extractBits meField 36 1) 0
        vrValue = extractBits meField 37 9
        vr = if vrValue == 0
             then Nothing
             else Just $ (fromIntegral vrValue - 1) * 64 * (if vrSign then -1 else 1)
    in (vr, vrSource)

decodeGNSSBaroDiff :: BS.ByteString -> Maybe Double
decodeGNSSBaroDiff meField =
    let diffSign = testBit (extractBits meField 48 1) 0
        diffValue = extractBits meField 49 7
    in if diffValue == 0 || diffValue == 127
       then Nothing
       else Just $ fromIntegral diffValue * 25 * (if diffSign then -1 else 1)
