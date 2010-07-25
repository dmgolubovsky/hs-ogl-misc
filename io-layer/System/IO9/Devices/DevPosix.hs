------------------------------------------------------------------
-- |
-- Module      :  System.IO9.Devices.DevPosix
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Implementation of virtual device to access the host filesystem
------------------------------------------------------------------

module System.IO9.Devices.DevPosix (
  devPosix
) where

import Data.Word
import System.FilePath
import System.Directory
import System.IO9.Device
import qualified Data.Map as M

-- | Initialization of the device. The function returns a thunk holding a host
-- path that becomes a "root" of the device. If the path does not exist, the function
-- fails.

devPosix :: FilePath               -- ^ Host path to access via this device (preferrably
                                   -- absolute, but will be canonicalized)
         -> IO Device9P            -- ^ Function implementing the device

devPosix fp = do
  cfp <- canonicalizePath fp
  ex <- doesDirectoryExist cfp
  case ex of
    True -> return $ dp $ newdata cfp
    False -> return $ devError $ "Directory " ++ fp ++ "does not exist"

-- Device internal data.

data DevPosix = DevPosix {
  agreed :: Bool                   -- protocol version agreed
 ,hfp :: FilePath                  -- root of the host file path
 ,fidmap :: M.Map Word32 FilePath  -- map of FIDs to actual paths
}

-- Initialize device data.

newdata :: FilePath -> DevPosix

newdata fp = DevPosix False fp M.empty

-- Device function which takes 9P2000 requests and returns responses.

dp :: DevPosix -> Device9P

-- Version: ignore the message size for now, but only proceed 

dp devd msg@(Msg TTversion 0 tv@(Tversion {})) = case tv_version tv of
  "9P2000" -> return $ Resp9P (msg {msg_typ = TRversion}) (dp devd {agreed = True})

-- Any other message can only be responded if previously agreed on protocol version.

dp devd msg | agreed devd == False = devError "Protocol negotiation failure" msg

dp devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ "Invalid message " ++ show msg) (dp devd)

