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
 ,DirEntry (..)
 ,DevTop (..)
 ,DevGen
 ,devGen
 ,genTopDir
 ,genAttach
) where

import Data.List
import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import Data.Maybe
import Data.Either
import Data.IORef
import Control.Monad
import System.FilePath
import Control.Concurrent
import System.IO9.Error
import Control.Exception (throwIO)
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
  ,dt_entry :: DirEntry}                         -- ^ Entry itself

-- | A data type to represent an entry in a device directory.

data DirEntry = EmptyFile                        -- ^ An entry which is just a placeholder.
                                                 -- It is used when initializing the device
                                                 -- directory, and also when a concrete driver
                                                 -- provides its own handlers for file manipulation.
              | MemoryFile !(IORef B.ByteString) -- ^ A memory-backed file. This is default
                                                 -- implementation of file storage by the generic
                                                 -- driver. Its own file manipulation methods
                                                 -- operate on such memory-backed files.
              | DirMap (M.Map FilePath Int)      -- ^ For a directory entry, maintain a map
                                                 -- of names into file indices.

-- | A type alias for a device top directory. All objects that device has are indexed here.
-- A new object gets index one more than the maximum index in the map, so indices are never
-- reused. Each object's 'Qid' has its 'qid_path' field set to the object index (cast from
-- Int to Word64). The very top level represents a map of device subtrees.
-- Each device method holds a 'MVar' reference to the device top directory in its closure,
-- so access to it is properly serialized.

type DevTop = M.Map FilePath (I.IntMap DirTab)

-- | A type alias for a typical device initialization function.

type DevGen = [(FilePath, [DirTab])]             -- ^ Initial contents
            -> IO DevTable                       -- ^ Returns a populated device table
 

-- | Given a list of device subtrees and their initial contents, build a device table
-- whose methods are all default for a generic device. Concrete implementation may
-- override some later. Assigning 'qid_path' values is up to the calling program.
-- It is however required that the topmost directory of each subtree had the minimal
-- 'qid_path' value within the whole subtree. Values of 'qid_path' must be unique within
-- a subtree.

devGen :: MVar DevTop -> Char -> DevGen

devGen mtop c t = do
  let devtbl = (defDevTable c) {
    attach_ = genAttach devtbl mtop}
  return devtbl

-- | Build the device toplevel directory and store it in a 'MVar'.
    
genTopDir :: [(FilePath, [DirTab])]
          -> IO (MVar DevTop)

genTopDir t = do
  let top = M.fromList (map mktree t)
      mktree (fp, dts) = (fp, I.fromList $ zip (map (fromIntegral . qid_path . dt_qid) dts) dts)
  newMVar top

-- Methods of the generic device. A concrete implementation will call devGen first, 
-- then override any method it needs, calling methods from the default table when needed. 
-- Each method of a device derived from this generic driver must take two
-- extra left arguments besides those mentioned in DevLayer definitions: reference to the device
-- table itself (in order to place its reference to any DevAttach it creates), and the MVar
-- pointing to the top directory: it is shared across all methods, and if one method updates it,
-- others can see. All methods should be coded in exception-safe manner in the sense that
-- the MVar pointing to the device top directory should be released even on exception.
-- withMVar and modifyMVar (if modifying the directory) are recommended. In such case a method
-- may just throw an exception or error when needed not worrying about releasing the top directory.

-- Attach the device at the given subtree. The generic implementation fails if the tree
-- does not exist (was not specified when instantiating the device). Concrete implementation
-- may create subtrees on an ad-hoc basis. Device subtree name should not contain slashes
-- unless it is "/".

genAttach :: DevTable                            -- device table to store in the result
          -> MVar DevTop                         -- mutable reference to the top directory
          -> ProcPriv                            -- attachment privileges, as come from NS layer
          -> FilePath                            -- device subtree (should not contain slashes)
          -> IO DevAttach                        -- result

genAttach tbl mvtop priv tree | '/' `elem` tree && tree /= "/" = throwIO Ebadchar

genAttach tbl mvtop priv tree = withMVar mvtop $ \top -> do
  let mbtopdir = M.lookup tree top               -- try to find the subtree
  case mbtopdir of
    Nothing -> throwIO Ebadarg                   -- does not exist
    Just topdir -> do
      when (I.null topdir) $ throwIO Enonexist   -- subtree was empty
      let tddir = snd $ I.findMin topdir         -- top dir entry
          tdqid = dt_qid tddir
      when (qid_typ tdqid .&. c_QTDIR == 0) $    -- check if this is indeed a directory
        throwIO Enotdir                          -- error if not
      return DevAttach { devtbl = tbl            -- copy the device table
                        ,devpriv = priv          -- copy the attach privileges
                        ,devqid = tdqid          -- QID of the top dir
                        ,devpath = "/"           -- at the subtree root
                        ,devtree = tree}         -- copy subtree name
        


