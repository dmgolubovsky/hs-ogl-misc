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
 ,ProcPriv (..)
 ,defDevTable
 ,devAttach
 ,devWalk
 ,devOpen
 ,devStat
 ,devWstat
 ,devRemove
 ,devCreate
 ,isDevice
) where

import Data.List
import Data.Word
import Data.Bits
import Data.NineP
import Data.NineP.Bits
import Data.NineP.Posix
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
  ,attach_ :: ProcPriv -> FilePath -> IO DevAttach   -- ^ Attach a device with the given root
  ,walk_ :: DevAttach -> FilePath -> IO DevAttach    -- ^ Walk to the object with given name 
                                                     --   (file or directory) any # levels
  ,open_ :: DevAttach -> Word8 -> IO Handle          -- ^ Open a handle on the given object
  ,create_ :: DevAttach -> FilePath -> Word32 -> IO DevAttach -- ^ Create a new object, w/o opening
  ,remove_ :: DevAttach -> IO ()                     -- ^ Remove the object referred to by the
                                                     --   'DevAttach' provided
  ,stat_ :: DevAttach -> IO Stat                     -- ^ Obtain object attributes
  ,wstat_ :: DevAttach -> Stat -> IO DevAttach       -- ^ Change some object attributes
}

-- | Device attachment. This data structure represents a file or a directory
-- on the given device.

data DevAttach = DevAttach {
   devtbl :: DevTable                            -- ^ Device which created this 'DevAttach'
  ,devpriv :: ProcPriv                           -- ^ Attachment privileges (copied from process)
  ,devqid :: Qid                                 -- ^ Atributes of this object
  ,devpath :: FilePath                           -- ^ Path to this object from the server root
  ,devtree :: FilePath                           -- ^ Device tree (between # and /)
}

-- | Thread privileges. This structure somehow should belong to each thread. When
-- a device is attached, it is placed into the attachment descriptor ('DevAttach').
-- It is entirely up to the server/driver to interpret this value and grant or deny
-- access to its resources.

data ProcPriv = Init                             -- ^ The initial process
              | Admin                            -- ^ Privileged administrative process
              | HostOwner String                 -- ^ Host owner (who booted the host) user
              | World String String              -- ^ Any external user woth name and group

-- Show the full device path of the object in the attachment.

instance Show DevAttach where
  show da = '#' : devchar (devtbl da) : tail (normalise ("x" ++ devtree da </> devpath da))

-- | Attach the given device and the root of its tree (use / by default).

devAttach :: DevTable -> ProcPriv -> FilePath -> IO DevAttach

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
-- must provide its own instances of IODevice and BufferedIO. The second argument of this
-- function is 9P2000 open mode.

devOpen :: DevAttach -> Word8 -> IO Handle

devOpen da = open_ (devtbl da) da

-- | Obtain a 'Stat' structure for the given attachment descriptor. Note that the structure
-- returned is not a Posix file status but rather 9P2000 file status.

devStat :: DevAttach -> IO Stat

devStat da = stat_ (devtbl da) da

-- | Change some attributes of a file or a directory described by the attachment
-- descriptor provided.

devWstat :: DevAttach -> Stat -> IO DevAttach

devWstat da = wstat_ (devtbl da) da

-- | Create a new object on the device.

devCreate :: DevAttach -> FilePath -> Word32 -> IO DevAttach

devCreate da = create_ (devtbl da) da

-- | Remove an object from the device.

devRemove :: DevAttach -> IO ()

devRemove da = remove_ (devtbl da) da

-- | Default device table with all device functions throwing an error.

defDevTable :: Char -> DevTable

defDevTable c = DevTable { devchar = c
                          ,attach_ = \_ _ -> throwIO notimpl
                          ,walk_ = \_ _ -> throwIO notimpl
                          ,open_ = \_ _ -> throwIO notimpl
                          ,create_ = \_ _ _ -> throwIO notimpl
                          ,remove_ = \_ -> throwIO notimpl
                          ,stat_ = \_ -> throwIO notimpl
                          ,wstat_ = \_ _ -> throwIO notimpl} where
  notimpl = OtherError "Device function not implemented"

-- | Return 'True' is the given path is a device path (starts with #).

isDevice :: FilePath -> Bool

isDevice ('#':_) = True
isDevice _ = False


