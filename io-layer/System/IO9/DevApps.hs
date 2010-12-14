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
 ,Filter (..)
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
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as C

-- | A general type for a filtering application.

type Filter m t = [Argument]                     -- ^ Application arguments
                -> Nesteratee t                  -- ^ Standard output
                              t                  -- ^ Standard input
                              (NameSpaceT m)     -- ^ Namespaced IO layer monad
                              NineError          -- ^ Completion code ('EmptyStatus' if OK)


-- | A general type for an embedded application. One kind of an application is a
-- 'Nesteratee' which will be fed from a binary 'Enumerator' (standard input)
-- and will send its results to a binary 'Iteratee' (standard output). This kind
-- of an application is basically a filter, and it may be binary or text-based.
--
-- Another kind of an application is just a monadic function evaluated at the
-- 'NameSpaceT' level. Applications written this way have much more flexibility than
-- filters, however they have to create 'Enumerators' and 'Iteratees' themselves.
--
-- Standard input and output will be provided by the parent process (or it will be 
-- /dev/cons for the initial process). Other input and output channels are available via
-- redirect arguments.

data Application m = 
    BinFilter (Filter m B.ByteString)            -- ^ Binary stream filter
  | TextFilter (Filter m T.Text)                 -- ^ Text stream filter
  | Monadic    ([Argument]                       -- ^ Aplication arguments
             -> NameSpaceT m NineError)          -- ^ Returned value

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
      qps = take napps [1 .. ]
      oneapp fp q = dirTab q 0o555 (BinConst $ C.fromString $ yaml fp)
      yaml fp = unlines [
        "---"
       ,"builtin: " ++ fp
       ,"..."]
      apptbl = [dirTab 0 0o555 (DirMap $ M.fromList $ zip names qps)] ++ 
                 zipWith oneapp names (map fromIntegral qps)
  mtop <- genTopDir [("/", apptbl)]
  gentbl <- devGen mtop 'α'
  return gentbl {devname = "apps"}


