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
import System.FilePath
import System.Directory
import System.IO9.Device
import System.Posix.Files
import Control.Concurrent
import qualified Data.Map as M

-- | Initialization of the device. The function returns a thunk holding a host
-- path that becomes a "root" of the device. If the path does not exist, the function
-- fails.

devPosix :: FilePath               -- ^ Host path to access via this device (preferrably
                                   -- absolute, but will be canonicalized)
         -> IO Device9P            -- ^ Function implementing the device

devPosix fp = do
  cfp <- canonicalizePath fp
  ex <- doesDirectoryExist cfp
  case ex of
    True -> return $ dp $ newdata cfp
    False -> return $ devError $ "Directory " ++ fp ++ "does not exist"

-- Device internal data.

data DevPosix = DevPosix {
  agreed :: Bool                   -- protocol version agreed
 ,hfp :: FilePath                  -- root of the host file path
 ,fidmap :: M.Map Word32 FilePath  -- map of FIDs to actual paths
 ,uname :: String                  -- name of the (authenticated) user
 ,thrid :: Maybe ThreadId          -- ID of the thread that attached
}

-- Initialize device data.

newdata :: FilePath -> DevPosix

newdata fp = DevPosix False fp M.empty "" Nothing

-- Device function which takes 9P2000 requests and returns responses.

dp :: DevPosix -> Device9P

-- Version: ignore the message size for now, but only proceed 

dp devd msg@(Msg TTversion tg tv@Tversion {}) = case tv_version tv of
  "9P2000" -> return $ Resp9P (msg {msg_typ = TRversion}) (dp devd {agreed = True})

-- Any other message can only be responded if previously agreed on protocol version.

dp devd msg | agreed devd == False = devError "Protocol negotiation failure" msg

-- Attach: afid is ignored. Fid and aname are written into the fidmap (aname is
-- prefixed with the stored root filepath, canonicalized, and checked to be within the root).
-- Uname as well as the current thread ID are stored in the device data.
-- Note that aname may also represent a regular file.

dp devd msg@(Msg TTattach tg ta@Tattach {}) = do
  let norm = normalise (hfp devd ++ tat_aname ta)
  tree <- canonicalizePath norm 
  ex1 <- doesFileExist tree
  ex2 <- doesDirectoryExist tree
  case isAbsolute (tat_aname ta) && (ex1 || ex2) && isSubdir (hfp devd) tree of
    False -> return $ Resp9P (errorMsg (msg_tag msg) $ "Invalid or non-existent path " ++ 
                                                      tat_aname ta) (dp devd)
    True -> do
      let fidmap' = M.insert (tat_fid ta) tree (fidmap devd)
      stat <- getFileStatus tree
      let isdir = isDirectory stat
          inode = fileID stat
          ctime = modificationTime stat
          qid = Qid {
            qid_typ = if isdir then fromIntegral c_QTDIR else 0
           ,qid_vers = round(realToFrac ctime)
           ,qid_path = fromIntegral inode
          }
      tid <- myThreadId
      return $ Resp9P (Msg TRattach tg (Rattach qid)) 
                      (dp devd { 
                         fidmap = fidmap'
                        ,uname = tat_uname ta
                        ,thrid = Just tid})


dp devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ "Invalid message " ++ show msg) (dp devd)

-- Check that the path2 is a subdirectory of path1 (or equal to path1).

isSubdir p1 p2 | equalFilePath p1 p2 = True

isSubdir p1 p2 =
  let sp1 = splitPath p1
      sp2 = splitPath p2
  in  issub p1 p2

issub [] _ = True

issub (p1:p1s) (p2:p2s) = p1 == p2 && issub p1s p2s
