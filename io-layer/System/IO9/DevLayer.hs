------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DevLayer
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Device Layer
------------------------------------------------------------------

module System.IO9.DevLayer (
  DevTable (..) 
 ,DevAttach (..)
 ,defDevTable
 ,devAttach
 ,devWalk
 ,devOpen
 ,stat2Qid
) where

import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import System.IO
import System.Posix.Files
import System.FilePath
import System.IO9.Error
import Control.Exception

-- | Device operations table. This is a collection of functions that
-- implement certain operations on a device. The 'DevTable' data type is itself
-- pure, however operations implementations may refer to some 'MVar' or 'TVar'
-- based mutable structures. Device operations either return when successful
-- or throw errors/exceptions.

data DevTable = DevTable {
   devchar :: Char                                   -- ^ Device character
  ,attach_ :: FilePath -> IO DevAttach               -- ^ Attach a device with the given root
  ,walk_ :: DevAttach -> FilePath -> IO DevAttach    -- ^ Walk to the object with given name 
                                                     -- (file or directory) - only one level
  ,open_ :: DevAttach -> FilePath -> Word8 -> IO Handle -- ^ Open a handle on the given object
  ,create_ :: DevAttach -> FilePath -> Word32 -> Word8 -> IO () -- ^ Create a new object
  ,stat_ :: DevAttach -> IO FileStatus               -- ^ Obtain object attributes
  ,wstat_ :: DevAttach -> FileStatus -> IO DevAttach -- ^ Change some object attributes
}

-- | Device attachment. This data structure represents a file or a directory
-- on the given device. It holds a reference to the attached device, however
-- device specific data is completely inaccessible due to forall definition.

data DevAttach = DevAttach {
   devtbl :: DevTable                            -- ^ Device which created this 'DevAttach'
  ,devqid :: Qid                                 -- ^ Atributes of this object
  ,devpath :: FilePath                           -- ^ Path to this object from the server root
  ,devtree :: FilePath                           -- ^ Device tree (between # and /)
}

-- Show the full device path of the object in the attachment.

instance Show DevAttach where
  show da = '#' : devchar (devtbl da) : (devtree da </> devpath da)

-- | Attach the given device and the root of its tree (use / by default).

devAttach :: DevTable -> FilePath -> IO DevAttach

devAttach = attach_

-- | Device operations dispatch via an attachment.
--
-- Walk from this object on the device to another object on the device with
-- the relative path provided. The source attachment descriptor should correspond 
-- to a directory. Walks to dot are handled without device driver involvement.

devWalk :: DevAttach -> FilePath -> IO DevAttach

devWalk da _ | qid_typ (devqid da) .&. c_QTDIR == 0 = throw Enotdir

devWalk da fp | isAbsolute fp = throw Efilename

devWalk da fp = walk' da (normalise fp) where 
  walk' da "." = return da
  walk' da "./" = return da
  walk' da fp = walk_ (devtbl da) da fp

-- | Open a 'Handle' to access the given object on the device. The 'Handle' will be either
-- a "standard" file handle, or a custom handle: in this case the underlying device driver
-- must provide its own instances of IODevice and BufferedIO. The third argument of this
-- function is 9P2000 open mode.

devOpen :: DevAttach -> FilePath -> Word8 -> IO Handle

devOpen da = open_ (devtbl da) da

-- | Default device table with all device functions throwing an error.

defDevTable :: Char -> DevTable

defDevTable c = DevTable { devchar = c
                          ,attach_ = \_ -> throwIO Eshutdown
                          ,walk_ = \_ _ -> throwIO Eshutdown
                          ,open_ = \_ _ _ -> throwIO Eshutdown
                          ,create_ = \_ _ _ _ -> throwIO Eshutdown
                          ,stat_ = \_ -> throwIO Eshutdown
                          ,wstat_ = \_ _ -> throwIO Eshutdown}

-- | Build a Qid from file status.

stat2Qid :: FileStatus -> Qid

stat2Qid stat =
  let isdir = isDirectory stat
      inode = fileID stat
      ctime = modificationTime stat
      qid = Qid {
        qid_typ = if isdir then c_QTDIR else 0
       ,qid_vers = round(realToFrac ctime)
       ,qid_path = fromIntegral inode
      }
  in  qid


