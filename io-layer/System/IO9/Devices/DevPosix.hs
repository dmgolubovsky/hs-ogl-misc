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
import Data.Bits
import Data.List
import Data.Maybe
import Data.Binary.Put
import Data.Binary.Get
import Data.Either.Unwrap
import System.FilePath
import System.Directory
import System.IO9.Device
import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Posix.IO
import System.IO
import GHC.IO.Device
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import qualified Control.Exception as E
import qualified Data.Map as M

-- | Initialization of the device. The function returns a thunk holding a host
-- path that becomes a "root" of the device. If the path does not exist, the function
-- fails.

devPosix :: Bool                   -- ^ True if device connection can be shared between threads
         -> FilePath               -- ^ Host path to access via this device (preferrably
                                   -- absolute, but will be canonicalized)
         -> IO Device9P            -- ^ Function implementing the device

devPosix shr fp = do
  cfp <- canonicalizePath fp
  ex <- doesDirectoryExist cfp
  case ex of
    True -> return $ dpvers $ newdata shr cfp
    False -> return $ devError $ "Directory " ++ fp ++ "does not exist"

-- Device internal data.

data DevPosix = DevPosix {
  hfp :: FilePath                              -- root of the host file path
 ,fidmap :: M.Map Word32 FilePath              -- map of FIDs to actual paths
 ,openmap :: M.Map Word32 (Word8, Either Handle [FilePath]) -- map of FIDs currently open
 ,uname :: String                              -- name of the (authenticated) user
 ,thrid :: Maybe ThreadId                      -- ID of the thread that attached
 ,devshr :: Bool                               -- Device connection can be shared
}

-- Initialize device data.

newdata :: Bool -> FilePath -> DevPosix

newdata shr fp = DevPosix fp M.empty M.empty "" Nothing shr

-- Process the "Version" message, then the "Auth" message. Any other
-- sequence results in error.

-- Version: ignore the message size for now, but only proceed if the protocol version
-- is "9P2000". Any message other than "Version" results in an error, but the same
-- device connection may be reused for protocol version negotiation again.

dpvers :: DevPosix -> Device9P

dpvers devd msg@(Msg TTversion tg tv@Tversion {}) | tv_version tv == "9P2000" =
  return $ Resp9P (msg {msg_typ = TRversion}) (dpatch devd)

dpvers devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ 
                                             "Protocol negotiation failure" ++ show msg) 
                                  (dpvers devd)

-- Attach: afid is ignored. Fid and aname are written into the fidmap (aname is
-- prefixed with the stored root filepath, canonicalized, and checked to be within the root).
-- Uname as well as the current thread ID are stored in the device data.
-- Uname is currently ignored by this implementation. Note that aname may also represent 
-- a regular file. If aname points to a non-existent file or directory, the operation
-- fails, but the device remains in negotiated state.

dpatch :: DevPosix -> Device9P

dpatch devd msg@(Msg TTattach tg ta@Tattach {}) = do
  let norm = normalise (hfp devd ++ "/" ++ tat_aname ta)
  tree <- canonicalizePath norm 
  ex <- fileExist tree
  case ex && isSubdir (hfp devd) tree of
    False -> return $ Resp9P (errorMsg (msg_tag msg) $ "Invalid or non-existent path " ++ 
                                                      tat_aname ta) (dpatch devd)
    True -> do
      let fidmap' = M.insert (tat_fid ta) tree (fidmap devd)
      stat <- getFileStatus tree
      let qid = stat2qid stat
      tid <- myThreadId
      return $ Resp9P (Msg TRattach tg (Rattach qid)) 
                      (dpacc devd { 
                         fidmap = fidmap'
                        ,uname = tat_uname ta
                        ,thrid = Just tid})

-- This device does not require authentication.

dpatch devd msg@(Msg TTauth tg ta) = 
                  return $ Resp9P (errorMsg (msg_tag msg) $ "Authentication not reauired") 
                                  (dpvers $ newdata (devshr devd) (hfp devd))


-- Any message other than Auth throws a unnegotiated device entry to the client.

dpatch devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ "Not attached") 
                                  (dpvers $ newdata (devshr devd) (hfp devd))

-- At this point we can accept walk/open/read/write/etc. messages. If the "shared" flag
-- is True, thread ID is not checked, and any thread may reuse the authenticated connection.
-- If it is False, and a request comes from the thread other authenticated thread, a fresh
-- unnegotiated device is returned. Messages like Version and Auth are not accepted at this point.
-- They will return a unnegotiated or unauthenticated device respectively.
-- The Flush message has no action as all operations are synchronous.

dpacc :: DevPosix -> Device9P

dpacc devd msg | msg_typ msg == TTversion = dpvers (newdata (devshr devd) (hfp devd)) msg

dpacc devd msg | msg_typ msg == TTauth = dpatch (newdata (devshr devd) (hfp devd)) msg
 
dpacc devd msg | msg_typ msg == TTflush = 
  return $ Resp9P (msg {msg_typ = TRflush, msg_body = Rflush}) (dpacc devd) 

dpacc devd msg = do
  let emsg x = return $ Resp9P (errorMsg (msg_tag msg) x) (dpacc devd)
  tid <- myThreadId
  if (devshr devd) && isJust (thrid devd) && (tid /= fromJust (thrid devd)) 
    then dpvers (newdata (devshr devd) (hfp devd)) msg
    else case (msg_typ msg, msg_body msg) of
      (TTstat, Tstat {}) -> do      -- if file/dir does not exist, return an error message
        let sfid = ts_fid $ msg_body msg
            spath = M.lookup sfid $ fidmap devd
        case spath of
          Nothing -> emsg $ "Incorrect fid: " ++ show sfid
          Just sfp -> do
            ex <- fileExist sfp     -- file could have disappeared
            case ex of
              False -> emsg $ "File/directory does not exist: " ++ sfp
              True -> do
                st <- getFileStatus sfp
                let fname = if normalise (sfp ++ "/") == normalise (hfp devd ++ "/") 
                              then "/"
                              else head . reverse $ splitPath sfp
                ret <- stat2stat st fname
                return $ Resp9P (msg {msg_typ = TRstat, msg_body = Rstat [ret]}) (dpacc devd)
      (TTclunk, Tclunk {}) -> do
        let clfid = tcl_fid $ msg_body msg
            rclunk dv = return $ Resp9P (msg {msg_typ = TRclunk, msg_body = Rclunk}) dv
        if clfid == c_NOFID         -- this special value causes all FIDs to be clunked,
          then do                   -- and the device returned to the unauthenticated state
            let fids = M.keys (fidmap devd)
            mapM_ (clunk devd) fids
            rclunk . dpvers $ newdata (devshr devd) (hfp devd)
          else do
            devd' <- clunk devd clfid
            rclunk . dpacc $ devd'
      (TTwalk, Twalk {}) -> do
        let twfid = tw_fid $ msg_body msg
            twnfid = tw_newfid $ msg_body msg
            twnames = tw_wnames $ msg_body msg
            twpath = M.lookup twfid $ fidmap devd
            twex = isJust twpath
            twopen = M.member twfid $ openmap devd
            twnused = M.member twnfid $ fidmap devd
            difffid = twfid /= twnfid
            rwalk q d = return $ Resp9P (msg {msg_typ = TRwalk, msg_body = Rwalk q}) (dpacc d)
        case (twex, twopen, twnused && difffid) of
          (False, _, _) -> emsg $ "Fid is invalid: " ++ show twfid
          (True, True, _) -> emsg $ "Fid is open: " ++ show twfid
          (True, False, True) -> emsg $ "New fid " ++ show twnfid ++ " is in use"
          (True, False, False) -> do
            (rpth, res) <- walk (hfp devd) (fromJust twpath) twnames []
            let fidmap' = M.insert twnfid rpth (fidmap devd)
            case (length twnames, length res) of
              (0, 1) -> rwalk res (devd {fidmap = fidmap'})
              (_, 1) -> emsg "first path element cannot be walked"
              (m, n) | n == m + 1 -> rwalk (tail res) (devd {fidmap = fidmap'})
              _ -> rwalk (tail res) devd
      (TTopen, Topen ofid omode) -> do -- for directory, just store its contents
        let opath = M.lookup ofid $ fidmap devd
        case opath of
          Nothing -> emsg $ "Incorrect fid: " ++ show ofid
          Just ofp -> do
            ex <- fileExist ofp     -- file could have disappeared
            case ex of
              False -> emsg $ "File/directory does not exist: " ++ ofp
              True -> do
                let dot "." = True
                    dot ".." = True
                    dot _ = False
                st <- getFileStatus ofp
                oval <- if isDirectory st
                          then getDirectoryContents ofp >>= 
                               return . filter (not . dot) >>= 
                               return . Right
                          else openFd ofp (mod2mod $ omode .&. 3) Nothing (mod2flg omode) >>=
                               fdToHandle >>=
                               return . Left
                let openmap' = M.insert ofid (omode, oval) (openmap devd)
                    oqid = stat2qid st
                    devd' = devd {openmap = openmap'}
                return $ Resp9P msg {msg_typ = TRopen, msg_body = Ropen oqid 0} (dpacc devd')
      (TTread, Tread rfid roff rcnt) -> do -- directory contents always reads entirely
        let rpath = M.lookup rfid $ fidmap devd
            rval = M.lookup rfid $ openmap devd
            rread b = return $ Resp9P (msg {msg_typ = TRread, msg_body = Rread b}) (dpacc devd)
        case rpath of
          Nothing -> emsg $ "Incorrect fid: " ++ show rfid
          Just rfp -> do
            ex <- fileExist rfp     -- file could have disappeared
            case ex of
              False -> emsg $ "File/directory does not exist: " ++ rfp
              True -> case rval of
                Nothing -> emsg $ "Fid " ++ show rfid ++ " was not open"
                Just (m, Left h) | m .&. 3 /= c_OWRITE -> do
                  hSeek h AbsoluteSeek (fromIntegral roff)
                  B.hGet h (fromIntegral rcnt) >>= rread
                Just (m, Right fps) | m .&. 3 == c_OREAD && roff == 0 -> do
                  let rfps = map (rfp </>) fps
                      fstz f = (getFileStatus f >>= \s -> return [(s, f)]) `catch` 
                                 (\_ -> return [])
                  sts <- mapM fstz rfps >>= return . concat
                  nsts <- zipWithM stat2stat (map fst sts) (map snd sts)
                  let bs = map (runPut . put) nsts -- get Stat for each file
                      cbs = B.concat bs            -- serialize each Stat and concat
                  rread cbs                        -- send whatever results from concatenation
                _ -> emsg $ "Incorrect fid mode: " ++ show rfid 
      _ -> emsg $ "Incorrect message " ++ show msg


-- Clunk one fid, update the internal data as needed.

clunk :: DevPosix -> Word32 -> IO DevPosix

clunk devd clfid = do
  let clmode = M.lookup clfid $ openmap devd
      clpath = M.lookup clfid $ fidmap devd
      (rm, cl) = case clmode of
        Nothing -> (False, False)
        Just (m, mbfd) -> ((m .&. c_ORCLOSE) /= 0, isLeft mbfd)
      openmap' = M.delete clfid $ openmap devd
      fidmap' = M.delete clfid $ fidmap devd
      devd' = devd {openmap = openmap', fidmap = fidmap'}
  when cl $ hClose (fromLeft . snd . fromJust $ clmode)
  when (rm && isJust clpath && fromJust clpath /= hfp devd) $ removeLink (fromJust clpath)
  return devd'

-- Walk the given path from the base step by step.

walk :: FilePath -> FilePath -> [FilePath] -> [Qid] -> IO (FilePath, [Qid])

walk root base fps fpqs = do
  nbase <- canonicalizePath base
  ex <- fileExist nbase
  case ex of
    False -> return ("", fpqs)
    True -> do
      let nbase' = case isSubdir root nbase of
            True -> nbase
            False -> root
      stat <- getFileStatus nbase'
      let qid = stat2qid stat
          nxt = fpqs ++ [qid]
      case fps of
        [] -> return (nbase', nxt)
        fph:fpt -> walk root (nbase' </> fph) fpt nxt
      
      
-- Convert a 9P2000 open mode to Posix open mode.  

mod2mod :: Word8 -> OpenMode

mod2mod omode | omode == c_OREAD = ReadOnly
              | omode == c_OWRITE = WriteOnly
              | omode == c_ORDWR = ReadWrite
              | otherwise = error $ "Incorrect open mode: " ++ show omode

-- Convert a 9P2000 open mode to Posix open flags (in fact only O_TRUNC is affected).

mod2flg :: Word8 -> OpenFileFlags

mod2flg omode | (omode .&. c_OTRUNC) /= 0 = defaultFileFlags {trunc = True}
              | otherwise = defaultFileFlags

-- Check that the path2 is a subdirectory of path1 (or equal to path1).

isSubdir p1 p2 | equalFilePath p1 p2 = True

isSubdir p1 p2 =
  let sp1 = splitPath p1
      sp2 = splitPath p2
  in  p1 `isPrefixOf` p2

-- Build a Qid from file status.

stat2qid :: FileStatus -> Qid

stat2qid stat =
  let isdir = isDirectory stat
      inode = fileID stat
      ctime = modificationTime stat
      qid = Qid {
        qid_typ = if isdir then c_QTDIR else 0
       ,qid_vers = round(realToFrac ctime)
       ,qid_path = fromIntegral inode
      }
  in  qid

-- Build a filemode mask in terms of the 9P definition.

stat2mode :: FileStatus -> Word32

stat2mode st =
  let umode = fileMode st
      oshift = 6
      gshift = 3
      wshift = 0
      permmap = [(ownerReadMode, c_DMREAD `shiftL` oshift)
                ,(ownerWriteMode, c_DMWRITE `shiftL` oshift)
                ,(ownerExecuteMode, c_DMEXEC `shiftL` oshift)
                ,(groupReadMode, c_DMREAD `shiftL` gshift)
                ,(groupWriteMode, c_DMWRITE `shiftL` gshift)
                ,(groupExecuteMode, c_DMEXEC `shiftL` gshift)
                ,(otherReadMode, c_DMREAD `shiftL` wshift)
                ,(otherWriteMode, c_DMWRITE `shiftL` wshift)
                ,(otherExecuteMode, c_DMEXEC `shiftL` wshift)
                ,(directoryMode, c_DMDIR)]
      nmode = foldl mbit 0 permmap
      mbit acc (umb, nmb) = case umb .&. umode of
        0 -> acc
        _ -> acc .|. nmb
  in  nmode

-- Convert a Unix stat record to 9P2000 stat record.

stat2stat :: FileStatus -> FilePath -> IO Stat

stat2stat st fname = do
  funame <- (getUserEntryForID (fileOwner st) >>= return . userName) `catch`
              (\_ -> return . show $ fileOwner st)
  fgroup <- (getGroupEntryForID (fileGroup st) >>= return . groupName) `catch`
              (\_ -> return . show $ fileGroup st)
  let qid = stat2qid st
      mode = stat2mode st
      ret = Stat {
        st_typ = 0    -- these are not filled in by the driver, but
       ,st_dev = 0    -- rather by the surrounding framework
       ,st_qid = qid
       ,st_mode = mode
       ,st_atime = round $ realToFrac $ accessTime st
       ,st_mtime = round $ realToFrac $ modificationTime st
       ,st_length = fromIntegral $ fileSize st
       ,st_name = fname
       ,st_uid = funame
       ,st_gid = fgroup
       ,st_muid = funame
      }
  return ret



