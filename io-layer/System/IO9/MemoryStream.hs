------------------------------------------------------------------
-- |
-- Module      :  System.IO9.MemoryStream
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Stream operations with objects stored in memory
------------------------------------------------------------------

module System.IO9.MemoryStream (
  openConstHandle) where

import Data.IORef
import Data.Typeable
import GHC.IO.Device
import GHC.IO.Handle
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import System.IO
import System.IO.Error
import GHC.IO.CSPHandle
import System.FilePath
import qualified Data.ByteString as B

-- Datatypes and instances to define a Handle to read from a constant.

data ConstDev = ConstDev {
  constBuf :: !B.ByteString
 ,constSP :: !(IORef ContSP)} deriving (Typeable)

instance IODevice ConstDev where
  ready _ write _ = return (not write)
  devType _ = return RegularFile
  close = constClose
  isSeekable _ = return True

instance CSPIO ConstDev where
  getsp = readIORef . constSP
  setsp = writeIORef . constSP
  initsp (ConstDev buf sp) = return (ContReady (readconst 0 buf))
  bufsize = const 1024

constClose (ConstDev buf sp) = do
  modifyIORef sp (const $ ContErr $ mkIOError eofErrorType
                                              "Handle was closed" Nothing Nothing)
  return ()

-- Stream processor function. If called with zero offset, it returns the whole constant
-- (the CSPIO implementation wil handle this). Its continuation will be called
-- with the offset equal to the length of the constant and will return EOF.

readconst :: Int -> B.ByteString -> ContSPFun

readconst off buf f x p a = case off of
  0 | B.null buf -> return ContEOF
  0 -> return $ ContBuff buf (readconst (B.length buf) B.empty)
  _ -> return $ ContEOF

-- | Open a 'Handle' to read from an immutable 'B.ByteString'.

openConstHandle :: FilePath -> B.ByteString -> IO Handle

openConstHandle fp bs = do
  sp <- newIORef $ ContReady (readconst 0 bs)
  let dd = StreamReader $ ConstDev bs sp
  mkFileHandle dd fp ReadMode Nothing nativeNewlineMode

