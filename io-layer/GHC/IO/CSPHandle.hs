------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.CSPHandle
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- IO Handles backed by continuation stream processors 
------------------------------------------------------------------

module GHC.IO.CSPHandle (
  ContSP (..)
 ,ContSPFun
 ,CSPIO (..)
 ,StreamReader (..)
 ,StreamWriter (..)
 ,fillBuf
) where

import Prelude hiding (catch)
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.Typeable
import GHC.Exception
import GHC.IO
import GHC.IO.Handle
import GHC.IO.Device
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import GHC.IO.Exception
import System.IO.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I

-- | Continuation-based Stream processor data structure. In the 'ContReady' state,
-- it contains a continuation to process the next portion of information. The 'ContEOF'
-- and 'ContErr' states are final with no recovery, hence no continuation is
-- offered in those states.

data ContSP =
   ContEOF                              -- ^ EOF reached
 | ContReady ContSPFun                  -- ^ Data available or can write data
 | ContBuff B.ByteString ContSPFun      -- ^ Input data buffered as it did not fit
                                        --   into the handle's buffer, or output
                                        --   data post-buffered as it is not 
                                        --   sufficient for a transmission unit
 | ContErr !IOException                 -- ^ Exception occurred


-- | Continuation-based Stream processor function to implement the handle.
-- A stream processor function has to be provided by the implementation of 'Handle'.

-- A read handle stream processor function will get only blocking I/O flag, and
-- available buffer space (as advisory only, may be zero). It is expected to
-- return 'ContBuff' as new stream processor state where the first argument of 
-- 'ContBuff' is the data received as a bytestring. If a read handle stream processor
-- function returns 'ContReady', in a non-blocking operation, zero read count
-- will be returned, in a blocking operation the function will be called again that is,
-- the calling process will not be resumed.



type ContSPFun = Bool                  -- ^ True if blocking I/O           
              -> Maybe B.ByteString    -- ^ Only for write handles, post-buffered data
              -> Maybe (Ptr Word8)     -- ^ Handle's buffer
              -> Int                   -- ^ Available bytes in the buffer
              -> IO ContSP             -- ^ New stream processor state

-- | General class of IODevices backed by stream processor. An instance of 'CSPIO' is
-- expected to hold a mutable reference ('IORef' or 'MVar') to the stream processor state,
-- so the 'setsp' method would save a new state. The 'initsp' method should initialize
-- the underlying data source (seek to the beginning of the stream) and to return
-- 'ContReady' with proper continuation function (so that the next I/O operation works
-- at the beginning of the stream). This method will also be called when repositioning
-- the previously consumed stream to the beginning (the stream may even be in 'ContErr'
-- or 'ContEOF' state).

class (IODevice a) => CSPIO a where
  getsp :: a -> IO ContSP
  setsp :: a -> ContSP -> IO ()
  initsp :: a -> IO ContSP
  bufsize :: a -> Int

-- | Wrapper newtype for streamed reader devices.

newtype (CSPIO a) => StreamReader a = StreamReader {unStreamR :: a} deriving (Typeable)

-- | Wrapper newtype for streamed writer devices.

newtype (CSPIO a) => StreamWriter a = StreamWriter {unStreamW :: a} deriving (Typeable)

-- Instance of IODevice for the wrapper: delegate all methods to the wrapped object.

instance (CSPIO a) => IODevice (StreamReader a) where
  ready = ready . unStreamR
  close = close . unStreamR
  isTerminal = isTerminal . unStreamR
  isSeekable = isSeekable . unStreamR
  seek = seekCSP . unStreamR
  tell = tell . unStreamR
  getSize = getSize . unStreamR
  setSize = setSize . unStreamR
  setEcho = setEcho . unStreamR
  getEcho = getEcho . unStreamR
  setRaw = setRaw . unStreamR
  devType = devType . unStreamR
  dup d = dup (unStreamR d) >>= return . StreamReader
  dup2 d1 d2 = dup2 (unStreamR d1) (unStreamR d2) >>= return . StreamReader

-- Instance of BufferedIO.

instance (CSPIO d) => BufferedIO (StreamReader d) where
  newBuffer d b = mkNewRBuffer d b
  emptyWriteBuffer d _ = throwIO unsupportedOperation
  flushWriteBuffer d _ = throwIO unsupportedOperation
  flushWriteBuffer0 d _ = throwIO unsupportedOperation
  fillReadBuffer = readHBuf . unStreamR
  fillReadBuffer0 = readHBuf0 . unStreamR

-- | General utility function to be used by reader stream processor implementations:
-- Fill a buffer from a bytestring as much as possible, returning remainder

fillBuf :: B.ByteString -> Ptr Word8 -> Int -> IO (Int, Maybe B.ByteString)
fillBuf bb@(I.PS ps s l) buf avl = do
  let bl = B.length bb
      bcpy xs xl = I.memcpy buf xs (fromIntegral xl)
  if bl <= avl
    then do
      withForeignPtr ps $ \pss -> bcpy (pss `plusPtr` s) bl
      return (bl, Nothing)
    else do
      let (I.PS ps1 s1 l1, bb2) = B.splitAt avl bb
      withForeignPtr ps1 $ \pss -> bcpy (pss `plusPtr` s1) avl
      return (avl, Just bb2)

-- Perform a seek (rewind). We only get here if isSeekable returns True.
-- However anything may happen, so result has to be checked. The 'initsp'
-- method will be called. If it fails with any error, this error will be
-- saved as the new stream state.

seekerr = ioError $  mkIOError illegalOperationErrorType
                               "Stream can only be seek'd to 0"
                               Nothing
                               Nothing


seekCSP :: (CSPIO d) => d -> SeekMode -> Integer -> IO ()

seekCSP _ SeekFromEnd _ = seekerr

seekCSP d RelativeSeek 0 = return ()

seekCSP d AbsoluteSeek 0 = do
  sp <- (initsp d) `catch` (return . ContErr)
  setsp d sp
  return ()

seekCSP _ _ _ = seekerr

-- Not exported.

readHBuf0 :: (CSPIO d) => d -> Buffer Word8 -> IO (Maybe Int, Buffer Word8)
readHBuf0 d buf = do
  sp <- getsp d
  case sp of
    ContEOF -> return (Nothing, buf)
    ContErr e -> throwIO e
    ContBuff bs fn -> withBuffer buf $ \ptr -> do
      (nbt, mbrm) <- fillBuf bs (ptr `plusPtr` bufR buf) (bufferAvailable buf)
      let sp' = case mbrm of
            Nothing -> ContReady fn
            Just rm -> ContBuff rm fn
      setsp d sp'
      return (Just nbt, buf {bufR = bufR buf + nbt})
    ContReady fn -> withBuffer buf $ \ptr -> do
      sp' <- fn False Nothing Nothing (bufferAvailable buf)
      setsp d sp'
      case sp' of
        ContReady _ -> return (Just 0, buf)
        _ -> readHBuf0 d buf

readHBuf :: (CSPIO d) => d -> Buffer Word8 -> IO (Int, Buffer Word8)
readHBuf d buf = do
  sp <- getsp d
  case sp of
    ContEOF -> return (0, buf)
    ContErr e -> throwIO e
    ContBuff bs fn -> withBuffer buf $ \ptr -> do
      (nbt, mbrm) <- fillBuf bs (ptr `plusPtr` bufR buf) (bufferAvailable buf)
      let sp' = case mbrm of
            Nothing -> ContReady fn
            Just rm -> ContBuff rm fn
      setsp d sp'
      return (nbt, buf {bufR = bufR buf + nbt})
    ContReady fn -> withBuffer buf $ \ptr -> do
      sp' <- fn True Nothing Nothing (bufferAvailable buf)
      setsp d sp'
      readHBuf d buf
    
mkNewWBuffer :: (CSPIO d) => (StreamWriter d) -> BufferState -> IO (Buffer Word8)
mkNewWBuffer (StreamWriter d) WriteBuffer = newByteBuffer (bufsize d) WriteBuffer
mkNewWBuffer _ ReadBuffer = throwIO unsupportedOperation

mkNewRBuffer :: (CSPIO d) => (StreamReader d) -> BufferState -> IO (Buffer Word8)
mkNewRBuffer (StreamReader d) ReadBuffer = newByteBuffer (bufsize d) ReadBuffer
mkNewRBuffer _ WriteBuffer = throwIO unsupportedOperation


