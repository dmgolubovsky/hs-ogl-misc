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
 ,DirStreamB
 ,readDirStreamB) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import Foreign.Marshal.Alloc
import System.Posix.Directory
import qualified Data.ByteString as B

openDirStreamB :: FilePath -> IO DirStreamB
openDirStreamB name =
  withCString name $ \s -> do
    dirp <- throwErrnoPathIfNull "openDirStream" name $ c_opendir s
    return (DirStreamB dirp)

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr ())


newtype DirStreamB = DirStreamB (Ptr ())

readDirStreamB :: DirStreamB -> IO B.ByteString
readDirStreamB (DirStreamB dirp) =
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

