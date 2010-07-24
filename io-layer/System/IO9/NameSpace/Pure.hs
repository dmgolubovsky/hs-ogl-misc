------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.Pure
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Plan9-style union directories and namespace operations - pure
------------------------------------------------------------------

module System.IO9.NameSpace.Pure (
  BoundDir (..)
 ,BindFlag (..)
 ,UnionDir (..)
 ,NameSpace
 ,newNameSpace
 ,unionDir
 ,setGlobal
 ,getGlobal
 ,addDevEntry
 ,getDevEntry
 ,bindAt
 ,unmountAt
 ,isDevice
 ,deviceOf
 ,bindDirAt) where

import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import System.IO9.Device
import Control.Monad
import qualified Data.DList as DL
import qualified Data.Map as M

-- Need this to keep compiler happy.

instance (Show a) => Show (DL.DList a) where
  show x = show (DL.toList x)

instance (Ord a) => Ord (DL.DList a) where
  compare x y = compare (DL.toList x) (DL.toList y)

instance (Eq a) => Eq (DL.DList a) where
  x == y = (DL.toList x) == (DL.toList y)

-- | A datatype to represent a component of a bound directory. Lists of bound directories
-- are stored in the namespace map as values.

data BoundDir = BoundDir {
  dirfp :: FilePath                -- ^ Absolute path to the actual directory
 ,dircr :: Bool                    -- ^ 'True' if creation of files is allowed
} deriving (Eq, Ord, Show)

-- | A datatype to represent binding mode: before/after/replace, and
-- the boolean flag to allow/prohibit creation of files in this actual directory
-- (the latter has nothing to do with actual directory permissions: it only affects
-- which directory in the union is chosen for creation of a new file).

data BindFlag = BindBefore Bool    -- ^ Bind before any directory bound to this point
              | BindAfter Bool     -- ^ Bind after any directory bound to this point
              | BindRepl           -- ^ Replace all bindings at this point with this directory
                deriving (Show)

-- | A datatype to hold bound directories in an ordered list.

data UnionDir = UnionDir (DL.DList BoundDir) deriving (Eq, Ord, Show)

-- | Create an union directory containing a single directory. file creation
-- is enabled by default.

unionDir :: FilePath -> UnionDir
unionDir fp = UnionDir (DL.singleton BoundDir {dirfp = fp, dircr = True})

-- | Bind an actual directory at the given union point.

bindDirAt :: UnionDir              -- ^ The union point where to bind a directory
          -> FilePath              -- ^ Actual file path to bind (not checked for existence)
          -> BindFlag              -- ^ Bind mode
          -> UnionDir              -- ^ Updated union point.
bindDirAt (UnionDir dl) fp bf = case bf of
  BindRepl -> unionDir fp
  BindBefore cr -> UnionDir $ DL.cons (BoundDir {dirfp = fp, dircr = cr}) dl
  BindAfter cr -> UnionDir $ DL.snoc dl (BoundDir {dirfp = fp, dircr = cr})

-- | A datatype to represent a namespace value.

data NsValue = UnionPoint UnionDir      -- ^ A union point
             | DeviceEntry Device9P     -- ^ Device entry: a function taking 9P2000 requests
                                        -- and returning 9p2000 responses with continuation
             | GlobalSetting String     -- ^ A global setting (not to be confused 
                                        -- with process environment)
               deriving (Eq, Ord, Show)

-- | A datattype to represent a namespace key.

data NsKey = NsPath FilePath            -- ^ File path of a union point as seen by threads
           | NsGlobal String            -- ^ Global setting
           | NsDevice Char              -- ^ Virtual device or host filesystem path
           deriving (Eq, Ord, Show)

-- | The namespace itself which is a map of 'NsKey's to 'NsValue's. Namespace data is stored at
-- thread-level (may be shared between several threads) in a mutable reference, so impure
-- namespace functions just update those references as needed.

type NameSpace = M.Map NsKey NsValue

-- | Create a new empty namespace.

newNameSpace :: NameSpace

newNameSpace = M.empty

-- | Update a global setting. New value is created in the map, existing value is updated.
-- Do not prepend a '$' to the setting name (will be added automatically).

setGlobal :: NameSpace -> String -> String -> NameSpace

setGlobal ns k v = M.insert (NsGlobal k) (GlobalSetting v) ns

-- | Retrieve a global setting (its most recent value). Monadic failure occurs if there is no
-- such setting. Do not prepend a '$' to the setting name.

getGlobal :: (Monad m) => String -> NameSpace -> m String

getGlobal k ns = case M.lookup (NsGlobal k) ns of
  Just (GlobalSetting s) -> return s
  _ -> fail $ "$" ++ k ++ " does not exist"


-- | Add a virtual device or host path prefix. Entries for each letter can be added,
-- but not replaced (operation fails if such entry already exists).

addDevEntry :: (Monad m) => Char -> Device9P -> NameSpace -> m NameSpace

addDevEntry c d ns = case M.lookup (NsDevice c) ns of
  Just _ -> fail $ "#" ++ [c] ++ " already exists"
  Nothing -> return $ M.insert (NsDevice c) (DeviceEntry d) ns

-- | Look up a device entry by a letter. Monadic failure occurs if entry is not found.

getDevEntry :: (Monad m) => Char -> NameSpace -> m Device9P

getDevEntry c ns = case M.lookup (NsDevice c) ns of
  Just (DeviceEntry d) -> return d
  _ -> fail $ "#" ++ [c] ++ " does not exist"


-- | Mount a filepath at a union point. An absolute file path has to be provided.
-- If the union point does not exist in the namespace given, it will be created
-- and will consist of the original directory and the mounted file or directory. 
-- If it does exist, the new path will be added to the union according to the flags.
-- Note. Although the pure function does not check whether the mount point exists,
-- this will be done by the IO part of the algorithm. It however checks if a device
-- entry exists if the path starts with '#'

bindAt :: (Monad m) => FilePath -> FilePath -> BindFlag -> NameSpace -> m NameSpace

bindAt fp fp2 bf ns | null fp2 || 
                      (not (isAbsolute fp2) && not (isDevice fp2)) || 
                      (not (isAbsolute fp)) = 
  fail $ fp2 ++ " is not an absolute or a device path"

bindAt fp fp2 bf ns | fp == fp2 = return ns

bindAt fp fp2 bf ns = do
  let pthval = case isDevice fp2 of
        True -> isJust . getDevEntry (deviceOf fp2) $ ns
        False -> True
  case pthval of
    False -> fail $ "device entry " ++ [deviceOf fp2] ++ " invalid"
    True -> do
      let mbup = M.lookup (NsPath fp) ns
      case mbup of
        Nothing -> 
          return $ M.insert (NsPath fp) (UnionPoint $ bindDirAt (unionDir fp) fp2 bf) ns
        Just (UnionPoint up) -> 
          return $ M.adjust (const $ UnionPoint $ bindDirAt up fp2 bf) (NsPath fp) ns
        _ -> fail $ "union point " ++ fp ++ " does not exist"

-- | Unmount a filepath at a union point. If an empty string is provided as the
-- filepath to unmount, the union point is removed from the namespace completely.
-- If some absolute or device filepath is provided, and it is indeed mounted at the
-- union point, it is removed from the union. Otherwise monadic failure occurs.

unmountAt :: (Monad m) => FilePath -> FilePath -> NameSpace -> m NameSpace

unmountAt fp fp2 ns = let mbup = M.lookup (NsPath fp) ns in
  case mbup of
    Nothing -> fail $ "union point " ++ fp ++ " does not exist"
    Just _ | null fp2 -> return $ M.delete (NsPath fp) ns
    Just (UnionPoint (UnionDir ud)) ->
      let bds = DL.toList ud
          (b1, b2) = partition ((== fp2) . dirfp) bds
      in  case b1 of
            [] -> fail $ fp2 ++ " was not mounted at " ++ fp
            _ -> return $ M.adjust (const . UnionPoint . UnionDir $ DL.fromList b2) (NsPath fp) ns
    _ -> fail $ "union point " ++ fp ++ " does not exist"

-- | Return 'True' is the given path is a device path (starts with #).

isDevice :: FilePath -> Bool
isDevice ('#':_) = True
isDevice _ = False

-- | Extract the device letter (if any) from the path. If this is not a device path,
-- the 0 character is returned.

deviceOf ('#':d:_) = d
deviceOf _ = chr 0

