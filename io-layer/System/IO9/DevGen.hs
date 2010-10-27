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
 ,devGen
 ,genTopDir
 ,genAttach
 ,genStat
 ,genSize
 ,genUGID
 ,genPerm
 ,dirTab
) where

import Data.Char
import Data.List
import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import Data.NineP.Posix
import Data.Maybe
import Data.Either
import Data.IORef
import Control.Monad
import System.IO
import System.FilePath
import Control.Concurrent
import System.IO9.Error
import System.IO9.MemoryStream
import Control.Exception (throw, throwIO)
import System.IO9.DevLayer
import System.Posix.Files
import GHC.IO.Handle
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
  ,dt_owner :: ProcPriv                          -- ^ Object owner
  ,dt_time :: Word32                             -- ^ Last modification time, if possible
  ,dt_perm :: Word32                             -- ^ Object permissions
  ,dt_entry :: DirEntry}                         -- ^ Entry itself

-- | A data type to represent an entry in a device directory.

data DirEntry = EmptyFile                        -- ^ An entry which is just a placeholder.
                                                 -- It is used when initializing the device
                                                 -- directory, and also when a concrete driver
                                                 -- provides its own handlers for file manipulation.
              | MemoryFile !(IORef B.ByteString) -- ^ A memory-backed read-writable file.
              | BinConst B.ByteString            -- ^ Binary constant (read-only)
              | DirMap (M.Map FilePath Int)      -- ^ For a directory entry, maintain a map
                                                 -- of names into file indices.
              | HostFile FilePath                -- ^ Host file (must exist and will be open as file)
              | HostHandle {hhr :: Maybe Handle  -- ^ Host handle for reading
                           ,hhw :: Maybe Handle} -- ^ Host handle for writing

-- | A type alias for a device top directory. All objects that device has are indexed here.
-- A new object gets index one more than the maximum index in the map, so indices are never
-- reused. Each object's 'Qid' has its 'qid_path' field set to the object index (cast from
-- Int to Word64). The very top level represents a map of device subtrees.
-- Each device method holds a 'MVar' reference to the device top directory in its closure,
-- so access to it is properly serialized.

type DevTop = M.Map FilePath (I.IntMap DirTab)

-- | Given a list of device subtrees and their initial contents, build a device table
-- whose methods are all default for a generic device. Concrete implementation may
-- override some later. Assigning 'qid_path' values is up to the calling program.
-- It is however required that the topmost directory of each subtree had the minimal
-- 'qid_path' value within the whole subtree. Values of 'qid_path' must be unique within
-- a subtree.

devGen :: MVar DevTop -> Char -> IO DevTable

devGen mtop c = do
  let devtbl = (defDevTable c) {
    attach_ = genAttach devtbl mtop
   ,walk_ = genWalk devtbl mtop
   ,open_ = genOpen devtbl mtop
   ,stat_ = genStat devtbl mtop}
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

-- | Attach the device at the given subtree. The generic implementation fails if the tree
-- does not exist (was not specified when instantiating the device). Concrete implementation
-- may create subtrees on an ad-hoc basis. Device subtree name should not contain slashes
-- unless it is "/".

genAttach :: DevTable                            -- ^ Device table to store in the result
          -> MVar DevTop                         -- ^ Mutable reference to the top directory
          -> ProcPriv                            -- ^ Attachment privileges, as come from NS layer
          -> FilePath                            -- ^ Device subtree (should not contain slashes)
          -> IO DevAttach                        -- ^ Result

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
        

-- | Walk the device to the desired file or directory.

genWalk :: DevTable                              -- ^ Device table to store in the result
        -> MVar DevTop                           -- ^ Mutable reference to the top directory
        -> DevAttach                             -- ^ Attachment to the starting directory
        -> FilePath                              -- ^ Destination, relative
        -> IO DevAttach                          -- ^ Attachment descriptor for the destination

genWalk tbl mvtop da fp | isAbsolute fp || isDevice fp || null fp = throwIO Ebadarg

genWalk tbl mvtop da fp = withMVar mvtop $ \top -> do
  let mbtopdir = M.lookup (devtree da) top       -- check if subtree exists
  case mbtopdir of
    Nothing -> throwIO Ebadarg                   -- subtree does not exist
    Just topdir -> do
      let fpsp = splitPath fp                    -- split the path into components
          (dq, nfp) = onestep topdir 
                              (qid_path $ devqid da) 
                              fpsp 
                              (splitPath $ devpath da) -- go stepwise
      return da { devqid = dq                    -- fill in new Qid
                 ,devpath = joinPath nfp}        -- normalized path

-- Walk one step along the given path.

onestep :: I.IntMap DirTab -> Word64 -> [FilePath] -> [FilePath] -> (Qid, [FilePath])

onestep dmap qpath [] acc = case I.lookup (fromIntegral qpath) dmap of
  Nothing -> throw Enonexist
  Just dt -> (dt_qid dt, acc)

onestep dmap qpath ("." : fps) acc = onestep dmap qpath fps acc -- skip dot and dot-slash
onestep dmap qpath ("./" : fps) acc = onestep dmap qpath fps acc

onestep dmap qpath (".." : fps) acc = onestep dmap qpath ("../" : fps) acc
onestep dmap qpath ("../" : fps) [] = onestep dmap qpath fps [] -- do not go above subtree root
onestep dmap qpath ("../" : fps) acc = onestep dmap qpath fps (reverse $ tail $ reverse acc)

onestep dmap qpath (fp : fps) acc = case I.lookup (fromIntegral qpath) dmap of
  Nothing -> throw Enonexist
  Just dt -> case dt_entry dt of
    DirMap emp -> case M.lookup (unslash fp) emp of
      Nothing -> throw Enonexist
      Just idx -> onestep dmap (fromIntegral idx) fps (acc ++ [fp])
    _ | null fps -> (dt_qid dt, acc ++ [fp])
    _ -> throw Enotdir

unslash fp = reverse (u (reverse fp)) where
  u ('/' : x) = x
  u z = z

-- | Retrieve a 'Stat' structure for the given attachment descriptor.

genStat :: DevTable                              -- ^ Device table to store in the result
        -> MVar DevTop                           -- ^ Mutable reference to the top directory
        -> DevAttach                             -- ^ File/directory whose 'Stat' is retrieved
        -> IO Stat                               -- ^ Result

genStat tbl mvtop da = withMVar mvtop $ \top -> do
  let mbtopdir = M.lookup (devtree da) top       -- check if subtree exists
  case mbtopdir of
    Nothing -> throwIO Ebadarg                   -- subtree does not exist
    Just topdir -> do
      let mbdt = I.lookup (fromIntegral $ qid_path $ devqid da) topdir
          dt = fromMaybe (throw Enonexist) mbdt
          fname = head $ reverse $ splitPath $ devpath da
          ug = genUGID (dt_owner dt)
      fsize <- genSize (dt_entry dt)
      return Stat {
         st_typ = fromIntegral $ ord $ devchar tbl
        ,st_dev = 0
        ,st_qid = dt_qid dt
        ,st_mode = dt_perm dt
        ,st_atime = dt_time dt
        ,st_mtime = dt_time dt
        ,st_length = fsize
        ,st_name = fname
        ,st_uid = fst ug
        ,st_gid = snd ug
        ,st_muid = fst ug}


-- | Open a handle for the attachment descriptor provided. The generic device provides
-- opening methods for all entry types except 'MemoryFile'. Concrete implementations
-- must override this method if they use this type of files.

genOpen :: DevTable                              -- ^ Device table to store in the result
        -> MVar DevTop                           -- ^ Mutable reference to the top directory
        -> DevAttach                             -- ^ File/directory whose 'Stat' is retrieved
        -> Word8                                 -- ^ Open mode
        -> IO Handle                             -- ^ Result 

genOpen tbl mvtop da om = withMVar mvtop $ \top -> do
  let mbtopdir = M.lookup (devtree da) top       -- check if subtree exists
  case mbtopdir of
    Nothing -> throwIO Ebadarg                   -- subtree does not exist
    Just topdir -> do
      let mbdt = I.lookup (fromIntegral $ qid_path $ devqid da) topdir
          dt = fromMaybe (throw Enonexist) mbdt
          om' = om .&. 3
      genPerm (devpriv da) (dt_perm dt) om'
      case dt_entry dt of
        HostHandle hr hw -> do                   -- open a host handle. Mode arg selects which one.
          let mbh = if om' == c_OREAD then hr else hw
              h = fromMaybe (throw Eperm) mbh
          return h
        BinConst bs -> openConstHandle (devpath da) bs
        HostFile fp -> do
          let iom = omode2IOMode om
          openFile fp iom
        _ -> throwIO $ OtherError "Open method not implemented"
      

-- | Check if the requested open mode is permissible. Error is thrown if not.
-- The general logic for local drivers/servers: user section corresponds to
-- hostowner rights; world section corresponds to everyone else rights;
-- "none" gets denied any access (servers should use authentication to obtain
-- proper attachment descriptors). Group permissions are ignored. All local
-- files are supposed to be owned by the hostowner whoever the name is.

genPerm :: ProcPriv -> Word32 -> Word8 -> IO ()

genPerm req perm om = do
  let rqug = genUGID req
      rqu = fst rqug
      wrt m | m == c_OREAD = False
            | m == c_OEXEC = False
            | otherwise = True
      rd  m | m == c_OREAD = True
            | otherwise = False
      exe m | m == c_OEXEC = True
            | otherwise = False
      oread = rqu == "~" && perm .&. (c_DMREAD `shiftL` oShift) /=0
      wread =               perm .&. (c_DMREAD `shiftL` wShift) /=0
      owrite = rqu == "~" && perm .&. (c_DMWRITE `shiftL` oShift) /=0
      wwrite =               perm .&. (c_DMWRITE `shiftL` gShift) /=0
      oexec = rqu == "~" && perm .&. (c_DMEXEC `shiftL` oShift) /=0
      wexec =               perm .&. (c_DMEXEC `shiftL` wShift) /=0
      allow = (wrt om && (owrite || wwrite)) ||
              (rd  om && (oread  || wread)) ||
              (exe om && (oexec  || wexec))
  unless (allow && rqu /= "none") $ throwIO Eperm
  return ()


-- | Given the device directory entry file body, determine its size.

genSize :: DirEntry -> IO Word64

genSize (MemoryFile ibs) = readIORef ibs >>= return . fromIntegral . B.length

genSize (BinConst b) = return $ fromIntegral $ B.length b

genSize (HostFile fp) = getFileStatus fp >>= return . fromIntegral . fileSize

genSize _ = return 0

-- | Given the file owner privileges, determine user and group id.

genUGID :: ProcPriv -> (String, String)

genUGID (World u g) = (u, g)                     -- random non-local user
genUGID None = ("none", "none")                  -- nobody (server processes run with this)
genUGID _ = ("~", "~")                           -- hostowner: will be substituted with actual name

-- | A helper function to create a 'DirTab' entry, with zero access time and
-- HostOwner as the owner. The directory bit is determined upon the last 'DirEntry' argument.

dirTab :: Word64                                 -- ^ Becomes 'qid_path'
       -> Word32                                 -- ^ Initial permissions
       -> DirEntry                               -- ^ Object body
       -> DirTab                                 -- ^ Result


dirTab path perm ent =
  let qt = case ent of
        DirMap _ -> c_QTDIR
        _ -> 0
  in  DirTab (Qid qt 0 path) HostOwner 0 perm ent

