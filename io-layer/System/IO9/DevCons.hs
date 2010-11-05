------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DevCons
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A virtual device to access the console (stdin/stdout)
------------------------------------------------------------------

module System.IO9.DevCons (
  devCons
) where

import System.Posix.User
import System.IO9.DevLayer
import System.IO9.DevGen
import Data.NineP.Bits
import System.IO
import Data.NineP
import Control.Exception
import System.Environment
import qualified Data.ByteString.UTF8 as C
import qualified Data.Map as M

devCons :: IO DevTable

devCons = do
  ow <- getLoginName `Control.Exception.catch` (\(e::SomeException) -> getEnv "USER")
  let contbl = [
        dirTab 0 0o555 (DirMap $ M.fromList [("cons", 1)
                                            ,("hostowner", 2)
                                            ,("null", 3)
                                            ,("zero", 4)])
       ,dirTab 1 0o600 (HostHandle {hhr = Just stdin, hhw = Just stdout})
       ,dirTab 2 0o444 (BinConst $ C.fromString ow)
       ,dirTab 3 0o222 (HostFile "/dev/null")
       ,dirTab 4 0o444 (HostFile "/dev/zero")]
  mtop <- genTopDir [("/", contbl)]
  gentbl <- devGen mtop 'c'
  return gentbl {devname = "cons"}

