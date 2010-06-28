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
 ,GBReader (..)
 ,GBWriter (..)
 ,fillBuf
) where

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
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I

-- | Continuation-based Stream processor data structure. In the 'ContAvail' state,
-- it contains a continuation to process the next portion of information. The 'ContEOF'
-- and 'ContErr' states are final with no recovery, hence no continuation is
-- offered in those states.

data ContSP =
   ContEOF                              -- ^ EOF reached
 | ContAvail ContSPFun                  -- ^ Data available or can write data
 | ContErr !IOException                 -- ^ Exception occurred

-- | Continuation-based Stream processor function to implement the handle.
-- A stream processor function has to be provided by the implementation of 'Handle'.
-- The stream processor function takes a pointer to a buffer and amount
-- of space available for input or information to output, the function
-- returns amount of information actually transreffed, and the new stream processor
-- state which may contain the continuation call to the same or other function.

type ContSPFun = Bool                  -- ^ True if blocking I/O
              -> Ptr Word8             -- ^ Handle's buffer
              -> Int                   -- ^ Available bytes in the buffer
              -> IO (Int, ContSP)      -- ^ (Actually transferred, new stream processor state)

-- | General class of IODevices backed by stream processor. An instance of 'CSPIO' is
-- expected to hold a mutable reference ('IORef' or 'MVar') to the stream processor state,
-- so the 'setsp' method would save a new state.

class (IODevice a) => CSPIO a where
  getsp :: a -> IO ContSP
  setsp :: a -> ContSP -> IO ()
  bufsize :: a -> Int

-- | Wrapper newtype for streamed reader devices.

newtype (CSPIO a) => GBReader a = GBReader {unGBR :: a} deriving (Typeable)

-- | Wrapper newtype for streamed writer devices.

newtype (CSPIO a) => GBWriter a = GBWriter {unGBW :: a} deriving (Typeable)

-- Instance of IODevice for the wrapper: delegate all methods to the wrapped object.

instance (CSPIO a) => IODevice (GBReader a) where
  ready = ready . unGBR
  close = close . unGBR
  isTerminal = isTerminal . unGBR
  isSeekable = isSeekable . unGBR
  seek = seek . unGBR
  tell = tell . unGBR
  getSize = getSize . unGBR
  setSize = setSize . unGBR
  setEcho = setEcho . unGBR
  getEcho = getEcho . unGBR
  setRaw = setRaw . unGBR
  devType = devType . unGBR
  dup d = dup (unGBR d) >>= return . GBReader
  dup2 d1 d2 = dup2 (unGBR d1) (unGBR d2) >>= return . GBReader

-- Instance of BufferedIO.

instance (CSPIO d) => BufferedIO (GBReader d) where
  newBuffer d b = mkNewRBuffer d b
  emptyWriteBuffer d _ = throwIO unsupportedOperation
  flushWriteBuffer d _ = throwIO unsupportedOperation
  flushWriteBuffer0 d _ = throwIO unsupportedOperation
  fillReadBuffer d b = readHBuf (unGBR d) b
  fillReadBuffer0 d b = readHBuf0 (unGBR d) b

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

-- Not exported.

readHBuf0 :: (CSPIO d) => d -> Buffer Word8 -> IO (Maybe Int, Buffer Word8)
readHBuf0 d buf = do
  sp <- getsp d
  case sp of
    ContEOF -> return (Nothing, buf)
    ContErr e -> throwIO e
    ContAvail fn -> withBuffer buf $ \ptr -> do
      (brd, sp') <- fn False (ptr `plusPtr` bufR buf) (bufferAvailable buf)
      setsp d sp'
      return (Just brd, buf {bufR = bufR buf + brd})

readHBuf :: (CSPIO d) => d -> Buffer Word8 -> IO (Int, Buffer Word8)
readHBuf d buf = do
  sp <- getsp d
  case sp of
    ContEOF -> return (0, buf)
    ContErr e -> throwIO e
    ContAvail fn -> withBuffer buf $ \ptr -> do
      (brd, sp') <- fn True (ptr `plusPtr` bufR buf) (bufferAvailable buf)
      setsp d sp'
      return (brd, buf {bufR = bufR buf + brd})
    
mkNewWBuffer :: (CSPIO d) => (GBWriter d) -> BufferState -> IO (Buffer Word8)
mkNewWBuffer (GBWriter d) WriteBuffer = newByteBuffer (bufsize d) WriteBuffer
mkNewWBuffer _ ReadBuffer = throwIO unsupportedOperation

mkNewRBuffer :: (CSPIO d) => (GBReader d) -> BufferState -> IO (Buffer Word8)
mkNewRBuffer (GBReader d) ReadBuffer = newByteBuffer (bufsize d) ReadBuffer
mkNewRBuffer _ WriteBuffer = throwIO unsupportedOperation


