module Main where

import Test.Hspec
import qualified Data.Vector.Storable as V
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import ADSB.Decoder
import ADSB.AircraftTracker
import Data.Maybe (isJust)

-- Helper functions
hexToVector :: String -> V.Vector Word8
hexToVector = V.fromList . map (read . ("0x" ++)) . chunksOf 2
  where
    chunksOf n [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)

main :: IO ()
main = hspec $ do
  describe "ADSB.Decoder" $ do
    describe "Basic decoding" $ do
      it "extracts bits correctly" $ do
        let testVec = V.fromList [0xAA, 0xBB, 0xCC, 0xDD] :: V.Vector Word8
        extractBits testVec 0 8 `shouldBe` 0xAA
        extractBits testVec 8 8 `shouldBe` 0xBB
        extractBits testVec 4 8 `shouldBe` 0xAB
        extractBits testVec 0 32 `shouldBe` 0xAABBCCDD

    describe "Message type decoding" $ do
      it "decodes aircraft identification message correctly" $ do
        let msg = hexToVector "8D4840D6202CC371C32CE0576098"
        let result = decodeMessage msg
        result `shouldSatisfy` isJust
        let Just decodedMsg = result
        msgtype decodedMsg `shouldBe` 17
        icaoAddr decodedMsg `shouldBe` (0x48, 0x40, 0xD6)
        typeCode decodedMsg `shouldBe` 4
        case messageData decodedMsg of
          AircraftIdentificationData cs -> cs `shouldBe` "KLM1023"
          _ -> expectationFailure "Expected AircraftIdentificationData"

      it "decodes airborne position messages correctly" $ do
        let msg0 = hexToVector "8D40621D58C382D690C8AC2863A7"
        let msg1 = hexToVector "8D40621D58C386435CC412692AD6"
        let result0 = decodeMessage msg0
        let result1 = decodeMessage msg1
        
        result0 `shouldSatisfy` isJust
        result1 `shouldSatisfy` isJust
        
        let Just decodedMsg0 = result0
        let Just decodedMsg1 = result1
        
        typeCode decodedMsg0 `shouldBe` 11
        typeCode decodedMsg1 `shouldBe` 11

        case (messageData decodedMsg0, messageData decodedMsg1) of
          (AirbornePositionData alt0 cpr0, AirbornePositionData alt1 cpr1) -> do
            alt0 `shouldSatisfy` (\alt -> isJust alt && abs (maybe 0 id alt - 38000) < 1)
            alt1 `shouldSatisfy` (\alt -> isJust alt && abs (maybe 0 id alt - 38000) < 1)
            cprOddEven cpr0 `shouldBe` False  -- Even message
            cprOddEven cpr1 `shouldBe` True   -- Odd message
            cprLat cpr0 `shouldSatisfy` (\lat -> lat >= 0 && lat < 1)
            cprLon cpr0 `shouldSatisfy` (\lon -> lon >= 0 && lon < 1)
            cprLat cpr1 `shouldSatisfy` (\lat -> lat >= 0 && lat < 1)
            cprLon cpr1 `shouldSatisfy` (\lon -> lon >= 0 && lon < 1)
          _ -> expectationFailure "Expected AirbornePositionData"

      it "decodes surface position message correctly" $ do
        let surfaceMsg = hexToVector "8C4841753A9A153237AEF0F275BE"
        let result = decodeMessage surfaceMsg
        result `shouldSatisfy` isJust
        let Just decodedMsg = result

        typeCode decodedMsg `shouldBe` 7

        case messageData decodedMsg of
          SurfacePositionData pos mov track -> do
            cprOddEven pos `shouldBe` True
            groundSpeed mov `shouldSatisfy` (\s -> isJust s && abs (maybe 0 id s - 17) < 0.01)
            trackValid track `shouldBe` True
            trackAngle track `shouldSatisfy` (\t -> isJust t && abs (maybe 0 id t - 92.8) < 0.1)
          _ -> expectationFailure "Expected SurfacePositionData"

      it "decodes airborne velocity messages correctly" $ do
        let msgA = hexToVector "8D485020994409940838175B284F"
        let msgB = hexToVector "8DA05F219B06B6AF189400CBC33F"
        
        let resultA = decodeMessage msgA
        let resultB = decodeMessage msgB
        
        resultA `shouldSatisfy` isJust
        resultB `shouldSatisfy` isJust
        
        let Just decodedMsgA = resultA
        let Just decodedMsgB = resultB
        
        typeCode decodedMsgA `shouldBe` 19
        typeCode decodedMsgB `shouldBe` 19

        case messageData decodedMsgA of
          AirborneVelocityData velocityData -> do
            velocityType velocityData `shouldBe` GroundSpeed
            speed velocityData `shouldSatisfy` (\s -> isJust s && abs (maybe 0 id s - 159) < 1)
            heading velocityData `shouldSatisfy` (\h -> isJust h && abs (maybe 0 id h - 182.88) < 0.1)
            verticalRate velocityData `shouldSatisfy` (\vr -> isJust vr && abs (maybe 0 id vr - (-832)) < 1)
          _ -> expectationFailure "Expected AirborneVelocityData for message A"

        case messageData decodedMsgB of
          AirborneVelocityData velocityData -> do
            velocityType velocityData `shouldBe` AirSpeed
            speed velocityData `shouldSatisfy` (\s -> isJust s && abs (maybe 0 id s - 375) < 0.1)
            heading velocityData `shouldSatisfy` (\h -> isJust h && abs (maybe 0 id h - 243.98) < 0.1)
            verticalRate velocityData `shouldSatisfy` (\vr -> isJust vr && abs (maybe 0 id vr - (-2304)) < 1)
          _ -> expectationFailure "Expected AirborneVelocityData for message B"

    describe "CPR decoding" $ do
      it "decodeCPRPosition correctly identifies even and odd messages" $ do
        let evenMsg = V.fromList [0x58, 0xC3, 0x82, 0xD6, 0x90, 0xC8, 0xAC]  -- Even message
        let oddMsg  = V.fromList [0x58, 0xC3, 0x86, 0x43, 0x5C, 0xC4, 0x12]  -- Odd message
        
        let evenCPR = decodeCPRPosition evenMsg
        let oddCPR  = decodeCPRPosition oddMsg
        
        cprOddEven evenCPR `shouldBe` False
        cprOddEven oddCPR  `shouldBe` True
        
        cprLat evenCPR `shouldSatisfy` (\lat -> lat >= 0 && lat < 1)
        cprLon evenCPR `shouldSatisfy` (\lon -> lon >= 0 && lon < 1)
        cprLat oddCPR  `shouldSatisfy` (\lat -> lat >= 0 && lat < 1)
        cprLon oddCPR  `shouldSatisfy` (\lon -> lon >= 0 && lon < 1)

  describe "ADSB.AircraftTracker" $ do
    describe "Aircraft map updates" $ do
      it "updates aircraft map correctly and calculates global position" $ do
        let msg0 = hexToVector "8D40621D58C382D690C8AC2863A7"
        let msg1 = hexToVector "8D40621D58C386435CC412692AD6"
        let emptyAircraftMap = Map.empty
        
        let Just decodedMsg0 = decodeMessage msg0
        let Just decodedMsg1 = decodeMessage msg1
        
        let aircraftMap1 = updateAircraft dummyTime emptyAircraftMap decodedMsg0
        let aircraftMap2 = updateAircraft dummyTime aircraftMap1 decodedMsg1
        
        Map.size aircraftMap2 `shouldBe` 1
        
        let maybeAircraft = Map.lookup (icaoAddr decodedMsg0) aircraftMap2
        maybeAircraft `shouldSatisfy` isJust
        
        let Just aircraft = maybeAircraft
        acICAO aircraft `shouldBe` icaoAddr decodedMsg0
        acEvenPosition aircraft `shouldSatisfy` isJust
        acOddPosition aircraft `shouldSatisfy` isJust
        acLastPosition aircraft `shouldSatisfy` isJust
        
        let Just (lat, lon) = acLastPosition aircraft
        abs (lat - 52.2572) `shouldSatisfy` (< 0.1)
        abs (lon - 3.91937) `shouldSatisfy` (< 0.1)

    describe "Altitude decoding" $ do
      it "decodes altitude correctly" $ do
        let msg = hexToVector "8D40621D58C382D690C8AC2863A7"
        let Just decodedMsg = decodeMessage msg
        let emptyAircraftMap = Map.empty
        let aircraftMap = updateAircraft dummyTime emptyAircraftMap decodedMsg
        
        let maybeAircraft = Map.lookup (icaoAddr decodedMsg) aircraftMap
        maybeAircraft `shouldSatisfy` isJust
        
        let Just aircraft = maybeAircraft
        acAltitude aircraft `shouldSatisfy` (\(Just alt) -> abs (alt - 38000) < 1)
