------------------------------------------------------------------
-- |
-- Module      :  System.IO9.Device
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Generic parts of virtual devices implementing 9P2000 locally
------------------------------------------------------------------

module System.IO9.Device (
  module Data.NineP
 ,Resp9P (..)
 ,Device9P
 ,errorMsg
 ,devError
 ,body2type
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
) where

import Data.Word
import Data.NineP

-- | A data type to encode responses from virtual devices. Such responses contain
-- the returned message and the continuation function to process the next request.

data Resp9P = Resp9P {
  re_msg :: Msg 
 ,re_cont :: Device9P 
} deriving (Show)

-- | A function type to implement a virtual device communicating over localized
-- version of the 9P2000 protocol in continuation style.

type Device9P = Msg -> IO Resp9P

-- Instances to keep other modules happy

instance Show Device9P where
  show _ = "Device9P"

instance Eq Device9P where
  _ == _ = True

instance Ord Device9P where
  compare _ _ = EQ

-- | A convenience function to fill out an error message to be sent in response.

errorMsg :: Word16 -> String -> Msg

errorMsg tag err = Msg {
   msg_typ = TRerror
  ,msg_tag = tag
  ,msg_body = Rerror {
    re_ename = err
  }
} 

-- | A device that always returns the given error message. This funvtion is usable
-- in the situation when a permanent error occurs on the device, and any message
-- should result in the same error.

devError :: String -> Device9P

devError err msg = do
  let errmsg = errorMsg (msg_tag msg) err
  return $ Resp9P errmsg (devError err)

-- | Automatically find a message type value for message body type value.
-- Works only for request messages.
-- NB This function better belongs to the NineP package itself

body2type :: VarMsg -> Tag

body2type Tversion {} = TTversion
body2type Tauth {} = TTauth
body2type Tattach {} = TTattach
body2type Tflush {} = TTflush
body2type Twalk {} = TTwalk
body2type Topen {} = TTopen
body2type Tcreate {} = TTcreate
body2type Tread {} = TTread
body2type Twrite {} = TTwrite
body2type Tclunk {} = TTclunk
body2type Tremove {} = TTremove
body2type Tstat {} = TTstat
body2type Twstat {} = TTwstat
body2type _ = XXX_TTerror

-- Constants used in 9P2000 messages excange: obtained by running HSFFIG
-- against the relevant portion of include/libc.h (from the Plan9 source tree).

c_DMAPPEND = 1073741824
c_DMAUTH = 134217728
c_DMDEVICE = 8388608
c_DMDIR = 2147483648
c_DMEXCL = 536870912
c_DMEXEC = 1
c_DMMOUNT = 268435456
c_DMNAMEDPIPE = 2097152
c_DMREAD = 4
c_DMSETGID = 262144
c_DMSETUID = 524288
c_DMSOCKET = 1048576
c_DMSYMLINK = 33554432
c_DMTMP = 67108864
c_DMWRITE = 2
c_QTAPPEND = 64
c_QTAUTH = 8
c_QTDIR = 128
c_QTEXCL = 32
c_QTFILE = 0
c_QTMOUNT = 16
c_QTSYMLINK = 2
c_QTTMP = 4
c_OREAD = 0
c_OWRITE = 1
c_ORDWR = 2
c_OEXEC = 3
c_OTRUNC = 16
c_OCEXEC = 32
c_ORCLOSE = 64
c_OEXCL	= 0x1000


