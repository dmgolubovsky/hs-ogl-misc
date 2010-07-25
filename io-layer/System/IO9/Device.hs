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

