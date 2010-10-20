------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DevGen
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Generic Virtual Device Driver Implementation
------------------------------------------------------------------

-- This module is intended as default implementation of a virtual device driver
-- allowing for unlimited file tree depth, and storing all its objects in memory
-- (ByteStrings). Concrete implementations may reuse code from this module
-- for their needs.
--
-- Virtual device drivers that use disk-backed files are recommended to reuse
-- code from the HostAccess driver.

module System.IO9.DevGen (
  DirTab (..)
 ,DevTop (..)
 ,devGen
) where

import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import Data.Either
import System.FilePath
import Control.Concurrent
import System.IO9.DevLayer
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.IntMap as I

-- | A data type to represent a device file or directory entry. Roughly corresponds
-- to Plan 9' DirTab structure. A File entry contains a 'B.ByteString's as file body
-- (hence no need to keep file length here). A Directory entry contains a 'M.Map'
-- from 'FilePath' to 'Int' to address objects belonging to the directory via the
-- device's toplevel index.

data DirTab = DirTab {
   dt_qid :: Qid                                 -- ^ Object Qid
  ,dt_perm :: Word32                             -- ^ Object permissions
  ,dt_entry :: Either B.ByteString               -- ^ For a file, its contents
                      (M.Map FilePath Int)}      -- ^ For a directory, list of names

-- | A type alias for a device top directory. All objects that device has are indexed here.
-- A new object gets index one more than the maximum index in the map, so indices are never
-- reused. Each object's 'Qid' has its 'qid_path' field set to the object index (cast from
-- Int to Word64). The very top level represents a map of device subtrees.
-- Each device method holds a 'MVar' reference to the device top directory in its closure,
-- so access to it is properly serialized.

type DevTop = M.Map FilePath (I.IntMap DirTab)

-- | Given a list of device subtrees and their initial contents, build a device table
-- whose methods are all default for a generic device. Concrete implementation may
-- override some later.

devGen :: Char                                   -- ^ Device letter
       -> [(FilePath, [DirTab])]                 -- ^ Initial contents
       -> IO DevTable

devGen c t = return $ defDevTable c   -- TBD


