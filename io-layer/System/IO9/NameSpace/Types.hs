------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.Types
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- NameSpace Layer and Monad Transformer - common type definitions
------------------------------------------------------------------

module System.IO9.NameSpace.Types (
   BoundDir (..)
  ,BindFlag (..)
  ,UnionDir (..)
  ,UnionPoint (..)
  ,NameSpace (..)
  ,DevMap (..)
  ,NsEnv (..)
  ,PathHandle (..)
) where

import System.IO9.DevLayer
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.DList as DL

-- Need this to keep compiler happy.

instance (Show a) => Show (DL.DList a) where
  show x = show (DL.toList x)

instance (Ord a) => Ord (DL.DList a) where
  compare x y = compare (DL.toList x) (DL.toList y)

instance (Eq a) => Eq (DL.DList a) where
  x == y = (DL.toList x) == (DL.toList y)

-- A datatype to represent a component of a bound directory. Lists of bound directories
-- are stored in the namespace map as values.

data BoundDir = BoundDir {
  dirph :: PathHandle              -- ^ Actual directory object information
 ,dirfl :: BindFlag                -- ^ Copy of the bind flags as they were supplied
 ,dircr :: Bool                    -- ^ 'True' if creation of files is allowed
} deriving (Eq, Ord, Show)

-- | A datatype to represent binding mode: before/after/replace, and
-- the boolean flag to allow/prohibit creation of files in this actual directory
-- (the latter has nothing to do with actual directory permissions: it only affects
-- which directory in the union is chosen for creation of a new file).

data BindFlag = BindBefore Bool    -- ^ Bind before any directory bound to this point
              | BindAfter Bool     -- ^ Bind after any directory bound to this point
              | BindRepl           -- ^ Replace all bindings at this point with this directory
                deriving (Eq, Ord, Show)

-- | A datatype to hold bound directories in an ordered list.

newtype UnionDir = UnionDir {unDir :: DL.DList BoundDir} deriving (Eq, Ord, Show)

-- Namespace is a map where evaluable file paths are keys, and evaluated file paths along with
-- union points are values.

data UnionPoint = UnionPoint UnionDir FilePath deriving (Show)

type NameSpace = M.Map FilePath UnionPoint

type DevMap = M.Map Char DevTable

-- Namespace execution environment consists of the kernel devices table
-- (immutable), and a namespace itself (mutable transactional variable).

data NsEnv = NsEnv {
   kdtbl :: DevMap
  ,nspace :: MVar NameSpace
}

-- | A semi-opaque data type to represent an evaluated path. Note that path handles
-- identify filesystem objects by their name (paths), so if a file or directory
-- gets renamed behind the scenes, 'PathHandle's associated with them may become
-- invalid (unless the attachment descriptor is passed to the file server,
-- and the server has some way to track its objects by the embedded 'Qid').

data PathHandle = PathHandle {
  phAttach :: DevAttach                 -- ^ Attachment desctiptor for the path if evaluated
 ,phCanon :: FilePath                   -- ^ Canonicalized (with dot-elements removed) path
} deriving (Show)

instance Eq PathHandle where
  p1 == p2 = phCanon p1 == phCanon p2

instance Ord PathHandle where
  compare p1 p2 = compare (phCanon p1) (phCanon p2)


