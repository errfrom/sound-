{-# LANGUAGE QuasiQuotes                        #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE DataKinds, KindSignatures          #-}

module Main where

import qualified Language.C.Inline          as C
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except      (ExceptT, runExceptT)
import           Control.Exception               (Exception, throw)
import           Data.Text                       (Text)
import           Data.Data

C.include "portaudio.h"

data PaConnectionStatus = PaInitialized | PaTerminated

data PaConnection (connStatus :: PaConnectionStatus) = MkConnection

type PaMonad a = ExceptT PaErrorCode IO a

initializePa :: PaMonad (PaConnection 'PaInitialized)
initializePa = do liftIO [C.exp| int { Pa_Initialize() } |]
                  return MkConnection

terminatePa :: PaConnection 'PaInitialized -> PaMonad (PaConnection 'PaTerminated)
terminatePa _ = do liftIO [C.exp| int { Pa_Terminate() } |]
                   return MkConnection

terminatePa' :: PaConnection 'PaInitialized -> PaMonad ()
terminatePa' = void . terminatePa

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

getDeviceCount :: PaConnection 'PaInitialized -> PaMonad Int
getDeviceCount _ = do
  numDevices <- liftIO $ fromIntegral <$> [C.exp| int { Pa_GetDeviceCount() } |]
  if numDevices < 0
    then throw (codeToPaError numDevices)
    else return numDevices

getDevices :: PaConnection 'PaInitialized -> Int -> PaMonad [PaDeviceInfo]
getDevices _ 0 = return []
getDevices _ devicesCount = undefined

main :: IO ()
main = do
  runExceptT $ do
    conn <- initializePa
    numDevices <- getDeviceCount conn
    liftIO $ print numDevices
    terminatePa' conn
  return ()
