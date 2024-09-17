{-# LANGUAGE ForeignFunctionInterface #-}
module RTLSDR.FFI where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import qualified Data.Vector.Storable as V
import Control.Exception (bracket, throwIO)
import RTLSDR.Types

foreign import ccall "rtlsdr_open_device"
    c_rtlsdr_open_device :: CInt -> IO CInt

foreign import ccall "rtlsdr_setup_device"
    c_rtlsdr_setup_device :: CUInt -> CUInt -> IO CInt

foreign import ccall "rtlsdr_read_samples"
    c_rtlsdr_read_samples :: Ptr CUChar -> CInt -> IO CInt

foreign import ccall "rtlsdr_set_frequency"
    c_rtlsdr_set_frequency :: CUInt -> IO CInt

foreign import ccall "rtlsdr_set_gain"
    c_rtlsdr_set_gain :: CInt -> IO CInt

foreign import ccall "rtlsdr_close_device"
    c_rtlsdr_close_device :: IO ()

foreign import ccall "wrapper_rtlsdr_get_tuner_gains"
    c_wrapper_rtlsdr_get_tuner_gains :: Ptr CInt -> IO CInt

foreign import ccall "wrapper_rtlsdr_set_freq_correction"
    c_wrapper_rtlsdr_set_freq_correction :: CInt -> IO CInt

foreign import ccall "wrapper_rtlsdr_set_direct_sampling"
    c_wrapper_rtlsdr_set_direct_sampling :: CInt -> IO CInt

-- Helper function for error handling
handleRTLSDRError :: IO CInt -> IO (Either String ())
handleRTLSDRError action = do
    result <- action
    return $ if result == 0
        then Right ()
        else Left "RTLSDR operation failed"

openDevice :: Int -> IO (Either String ())
openDevice deviceIndex = handleRTLSDRError $ c_rtlsdr_open_device (fromIntegral deviceIndex)

setupDevice :: DeviceParams -> IO (Either String ())
setupDevice params = handleRTLSDRError $ c_rtlsdr_setup_device 
    (fromIntegral $ sampleRate params) 
    (fromIntegral $ centerFreq params)

readSamples :: BufferSize -> IO (Either String (V.Vector CUChar))
readSamples bufferSize = 
    allocaArray bufferSize $ \buffer -> do
        bytesRead <- c_rtlsdr_read_samples buffer (fromIntegral bufferSize)
        if bytesRead > 0
            then Right <$> V.fromList <$> peekArray (fromIntegral bytesRead) buffer
            else return $ Left "Failed to read samples"

setCenterFreq :: Frequency -> IO (Either String ())
setCenterFreq freq = handleRTLSDRError $ c_rtlsdr_set_frequency (fromIntegral freq)

setGain :: Int -> IO (Either String ())
setGain gainValue = handleRTLSDRError $ c_rtlsdr_set_gain (fromIntegral gainValue)

closeDevice :: IO ()
closeDevice = c_rtlsdr_close_device

-- New function to get available gains
getTunerGains :: IO (Either String [Int])
getTunerGains = 
    allocaArray 32 $ \gainsPtr -> do
        count <- c_wrapper_rtlsdr_get_tuner_gains gainsPtr
        if count > 0
            then Right . map fromIntegral <$> peekArray (fromIntegral count) gainsPtr
            else return $ Left "Failed to get tuner gains"

-- New function to set frequency correction
setFreqCorrection :: Int -> IO (Either String ())
setFreqCorrection ppm = handleRTLSDRError $ c_wrapper_rtlsdr_set_freq_correction (fromIntegral ppm)

-- New function to enable or disable direct sampling
setDirectSampling :: Bool -> IO (Either String ())
setDirectSampling on = handleRTLSDRError $ c_wrapper_rtlsdr_set_direct_sampling (if on then 1 else 0)

-- Updated helper function to ensure device is closed after use
withRTLSDR :: IO a -> IO a
withRTLSDR action = bracket 
    (do
        result <- openDevice 0
        case result of
            Left err -> throwIO (userError err)
            Right _ -> return ()
    )
    (\_ -> closeDevice)
    (\_ -> action)

-- Example of using withRTLSDR
sampleRTLSDR :: DeviceParams -> BufferSize -> IO (Either String (V.Vector CUChar))
sampleRTLSDR params bufferSize = withRTLSDR $ do
    setupResult <- setupDevice params
    case setupResult of
        Left err -> return $ Left err
        Right _ -> readSamples bufferSize
