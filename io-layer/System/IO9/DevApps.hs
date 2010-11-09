------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DevApps
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A virtual device to access built-in applications
------------------------------------------------------------------

module System.IO9.DevApps (
  devApps
 ,Application (..)
 ,AppTable (..)
) where

import System.IO9.DevLayer
import System.IO9.DevGen
import System.IO9.Error
import Data.NineP.Bits
import System.IO
import Data.NineP
import Data.Nesteratee
import Control.Exception
import System.IO9.NameSpace.Util
import System.IO9.NameSpace.Types
import System.IO9.NameSpace.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as C


-- | A general type for an embedded application. A typical application is a
-- 'Nesteratee' which will be fed from a binary 'Enumerator' (standard input)
-- and will send its results to a binary 'Iteratee' (standard output). Standard
-- input and output will be provided by the parent process (or it will be /dev/cons
-- for the initial process). Other input and output channels are available via
-- redirect arguments.

type Application m = [Argument]                  -- ^ Application arguments
                  -> Nesteratee B.ByteString     -- ^ Binary standard output
                                B.ByteString     -- ^ Binary standard input
                                (NameSpaceT m)   -- ^ Namespaced IO layer monad
                                NineError        -- ^ Completion code ('Enoerror' if OK)


-- | Table of embedded applications to be provided when initializing the layer.

type AppTable m = M.Map FilePath (Application m)

-- | Create a virtual device to represent built-in applications. This is an internal device:
-- it is initialized automatically by the NameSpace layer upon startup, based on the
-- list of applications provided. For each embedded application, a simple YAML document
-- is stored in the form:
--
-- < ---
-- < builtin: appname
-- < ...
--
-- YAML is considered the notation to describe the way the NameSpaced layer programs are
-- run regarding their arguments, redirects, and namespaces.

devApps :: AppTable m -> IO DevTable

devApps at = do
  ow <- logName
  let names = M.keys at
      napps = length names
      qps = take napps [2 .. ]
      oneapp fp q = dirTab q 0o555 (BinConst $ C.fromString $ yaml fp)
      yaml fp = unlines [
        "---"
       ,"builtin: " ++ fp
       ,"..."]
      apptbl = [
        dirTab 0 0o555 (DirMap $ M.fromList [("bin", 1)])
       ,dirTab 1 0o555 (DirMap M.empty)] ++ zipWith oneapp names qps
  mtop <- genTopDir [("/", apptbl)]
  gentbl <- devGen mtop 'α'
  return gentbl {devname = "apps"}


