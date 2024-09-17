module ADSB.AircraftTracker
    ( Aircraft(..)
    , AircraftMap
    , updateAircraft
    , decodeGlobalAirbornePosition
    ) where

import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Applicative ((<|>))
import ADSB.Decoder

type AircraftMap = Map.Map ICAO Aircraft

data Aircraft = Aircraft
    { acICAO :: ICAO
    , acEvenPosition :: Maybe (CPRPosition, UTCTime)
    , acOddPosition :: Maybe (CPRPosition, UTCTime)
    , acLastPosition :: Maybe (Double, Double)  -- (latitude, longitude)
    , acAltitude :: Maybe Double
    , acVelocity :: Maybe Double
    , acHeading :: Maybe Double
    , acVerticalRate :: Maybe Double
    , acCallsign :: Maybe String
    } deriving (Show)

updateAircraft :: UTCTime -> AircraftMap -> ADSBMessage -> AircraftMap
updateAircraft currentTime acMap msg =
    let icao = icaoAddr msg
        aircraft = Map.findWithDefault (newAircraft icao) icao acMap
        updatedAircraft = case messageData msg of
            AirbornePositionData {airborneAltitude = alt, airbornePosition = pos} ->
                updatePosition currentTime alt pos aircraft
            AircraftIdentificationData {identCallsign = cs} ->
                aircraft { acCallsign = Just cs }
            AirborneVelocityData velocityData ->
                updateVelocity velocityData aircraft
            _ -> aircraft
    in Map.insert icao updatedAircraft acMap

newAircraft :: ICAO -> Aircraft
newAircraft icao = Aircraft
    { acICAO = icao
    , acEvenPosition = Nothing
    , acOddPosition = Nothing
    , acLastPosition = Nothing
    , acAltitude = Nothing
    , acVelocity = Nothing
    , acHeading = Nothing
    , acVerticalRate = Nothing
    , acCallsign = Nothing
    }

updatePosition :: UTCTime -> Maybe Double -> CPRPosition -> Aircraft -> Aircraft
updatePosition time mAlt pos aircraft =
    let (newEven, newOdd) = if cprOddEven pos
                            then (acEvenPosition aircraft, Just (pos, time))
                            else (Just (pos, time), acOddPosition aircraft)
        newAltitude = mAlt <|> acAltitude aircraft
        newPosition = calculateGlobalPosition newEven newOdd
    in aircraft { acEvenPosition = newEven
                , acOddPosition = newOdd
                , acLastPosition = newPosition <|> acLastPosition aircraft
                , acAltitude = newAltitude
                }

updateVelocity :: AirborneVelocityInfo -> Aircraft -> Aircraft
updateVelocity velocityInfo aircraft =
    aircraft { acVelocity = speed velocityInfo
             , acHeading = heading velocityInfo
             , acVerticalRate = verticalRate velocityInfo
             }

calculateGlobalPosition :: Maybe (CPRPosition, UTCTime) -> Maybe (CPRPosition, UTCTime) -> Maybe (Double, Double)
calculateGlobalPosition (Just (evenPos, evenTime)) (Just (oddPos, oddTime)) =
    if abs (diffUTCTime evenTime oddTime) > 10  -- 10 seconds threshold
    then Nothing
    else decodeGlobalAirbornePosition evenPos oddPos
calculateGlobalPosition _ _ = Nothing

decodeGlobalAirbornePosition :: CPRPosition -> CPRPosition -> Maybe (Double, Double)
decodeGlobalAirbornePosition evenPos oddPos
    | cprOddEven evenPos == cprOddEven oddPos = Nothing
    | otherwise = do
        let (evenLat, evenLon) = (cprLat evenPos, cprLon evenPos)
            (oddLat, oddLon) = (cprLat oddPos, cprLon oddPos)
        
        let j = fromIntegral . floor $ (59 * evenLat - 60 * oddLat + 0.5)
        let latEven = (360.0 / 60) * (mod' j 60 + evenLat)
        let latOdd = (360.0 / 59) * (mod' j 59 + oddLat)
        
        let lat = if cprOddEven oddPos then latOdd else latEven
        let lat' = if lat >= 270 then lat - 360 else lat

        let nlEven = nl lat'
        let nlOdd = nl lat'
        
        if nlEven /= nlOdd
            then Nothing
            else do
                let ni = fromIntegral nlEven
                let m = fromIntegral . floor $ (evenLon * (ni - 1) - oddLon * ni + 0.5)
                let lon = (360.0 / ni) * (mod' m ni + (if cprOddEven oddPos then oddLon else evenLon))
                let lon' = if lon >= 180 then lon - 360 else lon
                return (lat', lon')
  where
    mod' :: Double -> Double -> Double
    mod' a b = a - b * fromIntegral (floor (a / b))

    nl :: Double -> Int
    nl lat
        | abs lat >= 87 = 2
        | abs lat > 0 = floor $ (2 * pi) / acos (1 - (1 - cos (pi / (2 * 15))) / (cos (pi/180 * abs lat) ^ 2))
        | otherwise = 59
