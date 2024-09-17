module ADSB.Decoder.OperationStatus
    ( OperationStatus(..)
    , SubType(..)
    , CapacityClass(..)
    , OperationalMode(..)
    , decodeOperationStatus
    ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

import ADSB.Decoder.Util

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

decodeOperationStatus :: BS.ByteString -> OperationStatus
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
