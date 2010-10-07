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
 ,stat2Mode
 ,stat2Stat
 ,mode2Mode
 ,omode2IOMode
) where

import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import System.IO
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

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

-- Common map for Posix and 9P2000 mode flags.

oshift = 6
gshift = 3
wshift = 0

permmap :: [(FileMode, Word32)]

permmap = [(ownerReadMode, c_DMREAD `shiftL` oshift)
          ,(ownerWriteMode, c_DMWRITE `shiftL` oshift)
          ,(ownerExecuteMode, c_DMEXEC `shiftL` oshift)
          ,(groupReadMode, c_DMREAD `shiftL` gshift)
          ,(groupWriteMode, c_DMWRITE `shiftL` gshift)
          ,(groupExecuteMode, c_DMEXEC `shiftL` gshift)
          ,(otherReadMode, c_DMREAD `shiftL` wshift)
          ,(otherWriteMode, c_DMWRITE `shiftL` wshift)
          ,(otherExecuteMode, c_DMEXEC `shiftL` wshift)
          ,(directoryMode, c_DMDIR)]

-- | Build a filemode mask in terms of the 9P definition.

stat2Mode :: FileStatus -> Word32

stat2Mode st =
  let umode = fileMode st
      nmode = foldl mbit 0 permmap
      mbit acc (umb, nmb) = case umb .&. umode of
        0 -> acc
        _ -> acc .|. nmb
  in  nmode

-- | Convert 9P2000 mode mask to Posix 'fileMode' mask.

mode2Mode :: Word32 -> FileMode

mode2Mode nmode = foldl mbit 0 permmap where
  mbit acc (umb, nmb) = case nmb .&. nmode of
    0 -> acc
    _ -> acc .|. umb

-- | Convert a Unix stat record to 9P2000 stat record.

stat2Stat :: FileStatus -> FilePath -> IO Stat

stat2Stat st fname = do
  funame <- (getUserEntryForID (fileOwner st) >>= return . userName) `catch`
              (\_ -> return . show $ fileOwner st)
  fgroup <- (getGroupEntryForID (fileGroup st) >>= return . groupName) `catch`
              (\_ -> return . show $ fileGroup st)
  let qid = stat2Qid st
      mode = stat2Mode st
      ret = Stat {
        st_typ = 0    -- these are not filled in by the driver, but
       ,st_dev = 0    -- rather by the surrounding framework
       ,st_qid = qid
       ,st_mode = mode
       ,st_atime = round $ realToFrac $ accessTime st
       ,st_mtime = round $ realToFrac $ modificationTime st
       ,st_length = fromIntegral $ fileSize st
       ,st_name = fname
       ,st_uid = funame
       ,st_gid = fgroup
       ,st_muid = funame
      }
  return ret




