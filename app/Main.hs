{-# LANGUAGE QuasiQuotes                        #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE DataKinds, KindSignatures          #-}

module Main where

import qualified Language.C.Inline          as C
import           Control.Monad.Trans.Except      (ExceptT)
import           Control.Exception               (Exception)
import           Data.Data

C.include "portaudio.h"

data PaConnectionStatus = PaInitialized | PaClosed

type PaMonad (s :: PaConnectionStatus) a = ExceptT PaErrorCode IO a

initializePa :: PaMonad PaClosed -> PaMonad PaInitialized
initializePa _ = runExceptT

data PaErrorCode =
    PaNotInitialized
  | PaUnanticipatedHostError
  | PaInvalidChannelCount
  | PaInvalidSampleRate
  | PaInvalidDevice
  | PaInvalidFlag
  | PaSampleFormatNotSupported
  | PaBadIODeviceCombination
  | PaInsufficientMemory
  | PaBufferTooBig
  | PaBufferTooSmall
  | PaNullCallback
  | PaBadStreamPtr
  | PaTimedOut
  | PaInternalError
  | PaDeviceUnavailable
  | PaIncompatibleHostApiSpecificStreamInfo
  | PaStreamIsStopped
  | PaStreamIsNotStopped
  | PaInputOverflowed
  | PaOutputUnderflowed
  | PaHostApiNotFound
  | PaInvalidHostApi
  | PaCanNotReadFromACallbackStream
  | PaCanNotWriteToACallbackStream
  | PaCanNotReadFromAnOutputOnlyStream
  | PaCanNotWriteToAnInputOnlyStream
  | PaIncompatibleStreamHostApi
  | PaBadBufferPtr
  | PaError
  deriving (Show, Data, Exception)

codeToPaError :: Int -> PaErrorCode
codeToPaError code =
  let index = code + 10000
      constrs = dataTypeConstrs . dataTypeOf $ (undefined :: PaErrorCode)
  in if index < length constrs - 1 && index > 0
       then fromConstr (constrs !! index)
       else PaError

type PaTime        = Double
type PaDeviceInfo  = Int

data PaDeviceInfo = PaDeviceInfo
  { paDeviceName                     :: Text
  , paDeviceMaxInputChannels         :: Int
  , paDeviceMaxOutputChannels        :: Int
  , paDeviceDefaultLowInputLatency   :: PaTime
  , paDeviceDefaultLowOutputLatency  :: PaTime
  , paDeviceDefaultHighInputLatency  :: PaTime
  , paDeviceDefaultHighOutputLatency :: PaTime
  , paDeviceDefaultSampleRate        :: Double
  } deriving (Show)

getDeviceCount :: PaMonad PaInitialized Int
getDeviceCount = do
  numDevices <- liftIO $ fromIntegral <$> [C.exp| int { Pa_GetDeviceCount() } |]
  if numDevices < 0
    then return $ Left (codeToPaError numDevices)
    else return (Right numDevices)

getDevices :: Int -> PaMonad PaInitialized [PaDeviceInfo]
getDevices 0 = return []
getDevices devicesCount = undefined


main :: PaMonad
main = do
  numDevices <- getDeviceCount
  print numDevices
