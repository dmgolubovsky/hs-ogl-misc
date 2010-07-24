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

import System.FilePath
import System.Directory
import System.IO9.Device

-- | Initialization of the device. The function returns a thunk holding a host
-- path that becomes a "root" of the device. If the path does not exist, the function
-- fails.

devPosix :: FilePath -> IO Device9P

devPosix fp = devError 0 "Not implemented yet..."




