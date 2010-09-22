{-# LANGUAGE ForeignFunctionInterface #-}

------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DirStream
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Read directory stream into ByteString rather than into String
------------------------------------------------------------------

module System.IO9.DirStream (
  openDirStreamB
 ,rewindDirStreamB
 ,closeDirStreamB
 ,DirStreamB
 ,readDirStreamB) where

import Prelude hiding (catch)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.Typeable
import GHC.IO.Device
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import System.IO.Error
import System.Posix.Types
import Control.Exception
import Foreign.Marshal.Alloc
import System.Posix.Directory
import qualified Data.ByteString as B
import qualified GHC.IO.Device as R
import qualified Data.ByteString.Internal as I

-- Mainly copypaste from System.Posix.Directory. This stuff is needed
-- to provide a directory-backed handle.

-- | @openDirStreamB dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.

openDirStreamB :: FilePath -> IO DirStreamB
openDirStreamB name =
  withCString name $ \s -> do
    dirp <- throwErrnoPathIfNull "openDirStream" name $ c_opendir s
    return (DirStreamB (dirp, name))

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr ())

-- The type for directory stream differs from the one in System.Posix.Directory
-- as we need to store the directory path, so duplication of handle can be done.

newtype DirStreamB = DirStreamB ((Ptr (), FilePath)) deriving (Typeable)

-- | @readDirStreamB dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--   structure. The result is packed into a 'B.ByteString', so 
--   file name encoding (if any) is preserved. An empty 'B.ByteString'
--   is returned once the end of the stream is reached.

readDirStreamB :: DirStreamB -> IO B.ByteString
readDirStreamB (DirStreamB (dirp, _)) =
  alloca $ \ptr_dEnt -> loop ptr_dEnt
  where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
        then do dEnt <- peek ptr_dEnt
                if (dEnt == nullPtr)
                   then return B.empty
                   else do
                     entry <- (d_name dEnt >>= B.packCString)
                     c_freeDirEnt dEnt
                     return entry
        else do errno <- getErrno
                if (errno == eINTR) then loop ptr_dEnt else do
                let (Errno eo) = errno
                if (eo == 0)
                   then return B.empty
                   else throwErrno "readDirStreamB"


foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr () -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr () -> IO CString

-- | @rewindDirStreamB dp@ calls @rewinddir@ to reposition
--   the directory stream @dp@ at the beginning of the directory.

rewindDirStreamB :: DirStreamB -> IO ()
rewindDirStreamB (DirStreamB (dirp, _)) = c_rewinddir dirp

foreign import ccall unsafe "rewinddir"
   c_rewinddir :: Ptr () -> IO ()

-- | @closeDirStreamB dp@ calls @closedir@ to close
--   the directory stream @dp@.

closeDirStreamB :: DirStreamB -> IO ()
closeDirStreamB (DirStreamB (dirp, _)) = do
  throwErrnoIfMinus1_ "closeDirStreamB" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr () -> IO CInt

-- Needed to implement a dup.

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr () -> COff -> IO ()

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr () -> IO COff

-- Functions implementing a Handle, not exported.

-- Duplicate a handle. Find our position in the stream,
-- open a new DirStreamB for the same directory, seek to the
-- same position on the new stream. Dup2 is currently unsupported.

dupDirStreamB :: DirStreamB -> IO DirStreamB

dupDirStreamB (DirStreamB (dirp, name)) = do
  ns@(DirStreamB (dirp', _)) <- openDirStreamB name
  c_telldir dirp >>= c_seekdir dirp'
  return ns

-- Seek on the handle is only supported to the absolute zero position.

seekDirStreamB :: DirStreamB -> SeekMode -> Integer -> IO ()

seekDirStreamB ds AbsoluteSeek 0 = rewindDirStreamB ds
  

seekDirStreamB (DirStreamB (dirp, name)) _ _ = ioError $
  mkIOError illegalOperationErrorType
            "Directory handle only seeks to 0"
            Nothing
            (Just name)

-- Instances of DirStreamB used to implement a directory-backed handle.

instance IODevice DirStreamB where
  ready _ _ _ = return True
  devType _ = return Directory
  close = closeDirStreamB
  dup = dupDirStreamB
  seek = seekDirStreamB
  isSeekable _ = return True

-- A directory-backed handle provides a zero-delimited sequence of filenames (see #4317).
{-
dirBufferSize = 2048 -- arbitrarily chosen, but hopefully covers maximum filename length (255*5)

instance BufferedIO DirStreamB where
  newBuffer d b = newByteBuffer dirBufferSize ReadBuffer
  fillReadBuffer = R.read
  fillReadBuffer0 = R.readNonBlocking


instance RawIO DirStreamB where
  read d buf avl = do
    bs@(I.PS ps s l) <- readDirStreamB d
    case B.null bs of
      True -> return 0
      False -> do
        let bl = B.length bs
        I.memcpy buf (pss `plusPtr` s) bl
        return bl
    
-}

