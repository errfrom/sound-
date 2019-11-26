{-# LANGUAGE QuasiQuotes                        #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE ExtendedDefaultRules               #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE ScopedTypeVariables                #-}

module Main where

import qualified Language.C.Inline          as C
import           Foreign.C.Types                 (CInt)
import           Foreign.Ptr                     (Ptr)
import           Foreign.C.String                (peekCString)
import           Foreign.Storable                (Storable)
import qualified Foreign.Marshal.Alloc      as Ptr

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader      (ReaderT(..), ask)
import           Control.Exception               (Exception, throw)

import           Data.Typeable                   (cast)
import           Data.Data
import           Data.IORef
import           Data.Text                       (Text)
import           Data.String                     (IsString(..))
import qualified Data.List                  as L (partition)

default (Text)

C.include "portaudio.h" -- stack ghci --ghci-options='-fobject-code -O0 -lportaudio'
C.include "<string.h>"

type PointerConstraint a = (Storable a, Eq a, Typeable a)
data AnyPointer = forall a. PointerConstraint a => AnyPointer (Ptr a)

instance Eq AnyPointer where
  AnyPointer a == AnyPointer b = Just a == cast b

data Pa = Pa
 { heapPointers :: IORef [AnyPointer]
 }

type PaMonad a   = ReaderT Pa (ExceptT PaErrorCode IO) a
type PaError     = CInt

pmAlloc :: forall p. PointerConstraint p => IO (Ptr p) -> PaMonad (Ptr p)
pmAlloc funAlloc = do
  ptr <- liftIO funAlloc :: PaMonad (Ptr p)
  pa <- ask
  liftIO $ modifyIORef' (heapPointers pa) (\hp -> AnyPointer ptr : hp)
  return ptr

pmFree :: forall p. PointerConstraint p => Ptr p -> PaMonad ()
pmFree ptr = ask >>= \pa -> liftIO $ do
  hp <- readIORef (heapPointers pa)
  let (sameMemoryPointers, restPointers) = L.partition (\x -> x == AnyPointer ptr) hp
  case sameMemoryPointers of
    [] -> return ()
    (AnyPointer _:_) -> do
      Ptr.free ptr
      modifyIORef' (heapPointers pa) (\_ -> restPointers)
      return ()

pmFreeAll :: PaMonad ()
pmFreeAll = ask >>= \pa -> do
  hp <- liftIO (readIORef . heapPointers $ pa)
  mapM_ (\(AnyPointer ptr) -> pmFree ptr) hp

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

paMayThrow :: IO PaError -> PaMonad Int
paMayThrow action = liftIO action >>= \x ->
  let result = fromIntegral x
  in if result < 0
       then throw (codeToPaError result)
       else return result

runPaT :: PaMonad a -> IO (Either PaErrorCode a)
runPaT action = do
  hp <- newIORef mempty
  let pa = Pa hp
  runExceptT . flip runReaderT pa $ do
    _ <- paMayThrow [C.exp| int { Pa_Initialize() } |]
    result <- action
    _ <- paMayThrow [C.exp| int { Pa_Terminate()  } |]
    return result

data PaDeviceInfo = PaDeviceInfo
  { paDeviceIndex :: Int
  , paDeviceName  :: Text
  } deriving (Show)

getDeviceCount :: PaMonad Int
getDeviceCount = paMayThrow [C.exp| int { Pa_GetDeviceCount() } |]

getDevices :: Int -> PaMonad [PaDeviceInfo]
getDevices 0 = return []
getDevices devicesCount =
  let index = fromIntegral $ pred devicesCount
  in do
    ptrDeviceName <- pmAlloc Ptr.malloc

    _ <- liftIO $ [C.block| int {
      const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo( $(int index) );
      strcpy( $(char* ptrDeviceName), deviceInfo->name);
      return 0;
    }|]

    deviceName <- liftIO $ peekCString ptrDeviceName
    pmFree ptrDeviceName
    let deviceInfo = PaDeviceInfo (fromIntegral index) (fromString deviceName)

    xs <- getDevices (devicesCount - 1)
    return $ deviceInfo : xs

main :: IO ()
main = do
  _ <- runPaT $ do
    numDevices <- getDeviceCount
    devices <- getDevices numDevices
    liftIO $ print devices
  return ()
