------------------------------------------------------------------
-- |
-- Module      :  Data.NineP.Bits
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  Portable
-- 
-- Module providing bit constants used in the 9P2000 protocol.
-- Adapted from include/libc.h (Plan9 source tree)
-- 
------------------------------------------------------------------

module Data.NineP.Bits (
  calcPerm
 ,keepAllStat
 ,c_DMAPPEND
 ,c_DMAUTH
 ,c_DMDEVICE
 ,c_DMDIR
 ,c_DMEXCL
 ,c_DMEXEC
 ,c_DMMOUNT
 ,c_DMNAMEDPIPE
 ,c_DMREAD
 ,c_DMSETGID
 ,c_DMSETUID
 ,c_DMSOCKET
 ,c_DMSYMLINK
 ,c_DMTMP
 ,c_DMWRITE
 ,c_QTAPPEND
 ,c_QTAUTH
 ,c_QTDIR
 ,c_QTEXCL
 ,c_QTFILE
 ,c_QTMOUNT
 ,c_QTSYMLINK
 ,c_QTTMP
 ,c_OREAD
 ,c_OWRITE
 ,c_ORDWR
 ,c_OEXEC
 ,c_OTRUNC
 ,c_OCEXEC
 ,c_ORCLOSE
 ,c_OEXCL
 ,c_NOFID) where

import Data.Word
import Data.Bits
import Data.NineP

-- | Calculate permissions of a file or directory being created, as described
-- in man (5) open <http://man.cat-v.org/plan_9/5/open>. The c_DMDIR bit
-- in the new object permissions affects whether the rule for a file or a directory
-- is being applied.

calcPerm :: Word32                     -- ^ new object permissions
         -> Word32                     -- ^ directory permissions
         -> Word32                     -- ^ calculated permissions

calcPerm newperm dirperm = case newperm .&. c_DMDIR of
  0 -> newperm .&. ((complement 0o666) .|. (dirperm .&. 0o666))
  _ -> newperm .&. ((complement 0o777) .|. (dirperm .&. 0o777))

-- | A 'Stat' structure populated with values that, being passed to wstat
-- would not change the file or directory properties.

keepAllStat :: Stat

keepAllStat = Stat {
  st_typ = maxBound
 ,st_dev = maxBound
 ,st_qid = Qid maxBound maxBound maxBound
 ,st_mode = maxBound
 ,st_atime = maxBound
 ,st_mtime = maxBound
 ,st_length = maxBound
 ,st_name = ""
 ,st_uid = ""
 ,st_gid = ""
 ,st_muid = ""}

-- | A special FID value (~ 0) to use in the attach message without authentication, and
-- (as extension to the existing 9P2000 specification) in the clunk message to clunk all
-- FIDs, basically to shutdown the entire device instance.

c_NOFID :: Word32

c_NOFID = complement 0

-- Constants used in 9P2000 messages exchange: obtained by running HSFFIG
-- against the relevant portion of include/libc.h (from the Plan9 source tree).

c_DMAPPEND = 1073741824 :: Word32
c_DMAUTH = 134217728 :: Word32
c_DMDEVICE = 8388608 :: Word32
c_DMDIR = 2147483648 :: Word32
c_DMEXCL = 536870912 :: Word32
c_DMEXEC = 1 :: Word32
c_DMMOUNT = 268435456 :: Word32
c_DMNAMEDPIPE = 2097152 :: Word32
c_DMREAD = 4 :: Word32
c_DMSETGID = 262144 :: Word32
c_DMSETUID = 524288 :: Word32
c_DMSOCKET = 1048576 :: Word32
c_DMSYMLINK = 33554432 :: Word32
c_DMTMP = 67108864 :: Word32
c_DMWRITE = 2 :: Word32
c_QTAPPEND = 64 :: Word8
c_QTAUTH = 8 :: Word8
c_QTDIR = 128 :: Word8
c_QTEXCL = 32 :: Word8
c_QTFILE = 0 :: Word8
c_QTMOUNT = 16 :: Word8
c_QTSYMLINK = 2 :: Word8
c_QTTMP = 4 :: Word8
c_OREAD = 0 :: Word8
c_OWRITE = 1 :: Word8
c_ORDWR = 2 :: Word8
c_OEXEC = 3 :: Word8
c_OTRUNC = 16 :: Word8
c_OCEXEC = 32 :: Word8
c_ORCLOSE = 64 :: Word8
c_OEXCL	= 0x1000 :: Word32


