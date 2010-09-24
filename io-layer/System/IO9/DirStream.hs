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
 ,readDirStreamB
 ,openDirHandle) where

import Prelude hiding (catch)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.IORef
import Data.Typeable
import GHC.IO.Handle
import GHC.IO.Device
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import System.IO
import System.IO.Error
import System.Posix.Types
import Control.Exception
import Foreign.Marshal.Alloc
import System.Posix.Directory
import GHC.IO.CSPHandle
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
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

-- Instances to implement a directory-backed handle.

data DirDev = DirDev {
  dirST :: !DirStreamB
 ,dirSP :: !(IORef ContSP)} deriving (Typeable)

instance IODevice DirDev where
  ready _ write _ = return (not write)
  devType _ = return Directory
  close = dirClose
  isSeekable _ = return True

instance CSPIO DirDev where
  getsp = readIORef . dirSP
  setsp = writeIORef . dirSP
  initsp (DirDev ds sp) = rewindDirStreamB ds >> return (ContReady (readdir ds))
  bufsize = const 4096

dirClose (DirDev ds sp) = do
  modifyIORef sp (const $ ContErr $ mkIOError eofErrorType 
                                              "Handle was closed" Nothing Nothing)
  return ()

-- Stream processor function. It ignores the blocking/non-blocking more flag as
-- directory reads should not cause any delays on local dirs. It also ignores
-- the advisory buffer size because only one directory entry is read at a time.
-- A directory-backed handle provides a zero-delimited sequence of filenames (see #4317).

dot = C.pack "."
dotdot = C.pack ".."

readdir :: DirStreamB -> ContSPFun

readdir ds f x p a = do
  b <- readDirStreamB ds                           -- get one entry
  case B.null b of                                 -- check if end of stream
    True -> return ContEOF                         -- if yes indicate EOF
    False -> do
      if b `elem` [dot, dotdot] 
        then readdir ds f x p a                    -- skip dot and dotdot
        else return $ ContBuff (C.snoc b '\000')   -- append zero byte
                               (readdir ds)        -- and pass to the IO layer

-- | Open a handle to the given directory by name. It is not checked for being
-- a directory here, sho should be checked prior.

openDirHandle :: FilePath -> IO Handle

openDirHandle dfp = do
  ds <- openDirStreamB dfp
  sp <- newIORef $ ContReady (readdir ds)
  let dd = StreamReader $ DirDev ds sp
  mkFileHandle dd dfp ReadMode Nothing nativeNewlineMode


