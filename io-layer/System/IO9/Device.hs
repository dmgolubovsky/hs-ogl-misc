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
 ,Resp9P
 ,Device9P
 ,response
 ,devError
) where

import Data.Word
import Data.NineP

-- | A data type to encode responses from virtual devices. Such responses contain
-- the returned message and the continuation function to process the next request.

data Resp9P = Resp9P Msg (IO Device9P)

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

-- | A convenience function that constructs a device response and returns
-- it into the IO monad.

response :: Msg -> (IO Device9P) -> IO Resp9P

response m iod = return $ Resp9P m iod

-- | A device that always returns the given error message. This funvtion is usable
-- in the situation when a permanent error occurs on the device, and any message
-- should result in the same error.

devError :: Word16 -> String -> IO Device9P

devError tag err = do
  let errmsg = Msg {
        msg_typ = TRerror
       ,msg_tag = tag
       ,msg_body = Rerror {
          re_ename = err
        }
      }
  return $ \_ -> response errmsg (devError tag err)

