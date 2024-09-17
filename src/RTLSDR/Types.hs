module RTLSDR.Types where

import Data.Complex
import Data.Int (Int16)
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CUChar)

type Frequency = Int
type SampleRate = Int
type BufferSize = Int

data DeviceParams = DeviceParams
    { sampleRate :: SampleRate
    , centerFreq :: Frequency
    , gain :: Int
    , freqCorrection :: Int
    , directSampling :: Bool
    } deriving (Show)

defaultDeviceParams :: DeviceParams
defaultDeviceParams = DeviceParams
    { sampleRate = 2048000  -- 2.048 MHz
    , centerFreq = 100000000  -- 100 MHz
    , gain = 0  -- Auto gain
    , freqCorrection = 0
    , directSampling = False
    }

defaultBufferSize :: BufferSize
defaultBufferSize = 256 * 1024  -- 256k samples

type Sample = Complex Double
type SampleBuffer = V.Vector Sample
type RawBuffer = V.Vector CUChar

-- Helper function to convert raw samples to complex samples
rawToComplex :: RawBuffer -> SampleBuffer
rawToComplex raw = V.generate (V.length raw `div` 2) $ \i ->
    let i' = i * 2
        real = (fromIntegral (rawToSigned (raw V.! i')) :: Double) / 127.5 - 1
        imag = (fromIntegral (rawToSigned (raw V.! (i' + 1))) :: Double) / 127.5 - 1
    in real :+ imag
  where
    rawToSigned :: CUChar -> Int16
    rawToSigned x = if x >= 128 then fromIntegral x - 256 else fromIntegral x
