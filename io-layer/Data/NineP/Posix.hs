------------------------------------------------------------------
-- |
-- Module      :  Data.NineP.Posix
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  Portable
-- 
-- Module providing converison between 9P2000 and Posix data structures.
-- 
------------------------------------------------------------------

module Data.NineP.Posix (
  stat2Qid
 ,omode2IOMode
) where

import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import System.IO
import System.Posix.IO
import System.Posix.Files

-- | Build a Qid from Posix file status.

stat2Qid :: FileStatus -> Qid

stat2Qid stat =
  let isdir = isDirectory stat
      inode = fileID stat
      ctime = modificationTime stat
      qid = Qid {
        qid_typ = if isdir then c_QTDIR else 0
       ,qid_vers = round(realToFrac ctime)
       ,qid_path = fromIntegral inode
      }
  in  qid

-- | Convert 9P2000 open mode to Posix open mode. OCEXEC and ORCLOSE bits
-- are ignored; OWRITE | OTRUNC results in 'WriteMode' while OWRITE alone in
-- AppendMode.

omode2IOMode :: Word8 -> IOMode

omode2IOMode m = f (m .&. (complement (c_OCEXEC .|. c_ORCLOSE)))
  where f m | m == c_OREAD = ReadMode
            | m == c_OWRITE .|. c_OTRUNC = WriteMode
            | m == c_OWRITE = AppendMode
            | m == c_ORDWR = ReadWriteMode
            | otherwise = ReadMode

