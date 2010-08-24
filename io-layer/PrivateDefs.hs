------------------------------------------------------------------
-- |
-- Module      :  PrivateDefs
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Private definitions: this module is not exposed
------------------------------------------------------------------

module PrivateDefs (
  ThreadState (..)
 ,ThreadCompl (..)
 ,NineM
 ,NameSpaceM
 ,Scope (..)
 ,Device (..)
 ,DEVFID
) where 

import System.IO9.Device
import System.IO9.NameSpace.Pure
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as I

-- | A newtype wrapper for a device reference (just a number indeed). It is opaque
-- to the external code that uses this monad.

newtype Device = Device {devRef :: Int} deriving (Eq, Ord)

instance Show Device where
  show d = "Device: #" ++ show (devRef d)

-- | Type synonym for a device-fid pair.

type DEVFID = (Device, FID)

-- Internal structure of a thread state.

data ThreadState u = ThreadState {
  intGen :: Int                              -- Integer number incremental generator
                                             -- to use for tags, fids, device indices, etc.
 ,devMap :: I.IntMap (Device9P, Char)        -- Device reference map. Each time a device is 
                                             -- attached, a new index is generated, and the device
                                             -- is referenced by that index through this map.
 ,thrEnv :: M.Map String String              -- Thread's own environment needed at this level.
 ,devTab :: M.Map Char Device9P              -- Table of devices (by letter)
 ,userState :: u                             -- This thread's user state
 ,thrMap :: M.Map ThreadId                   -- Child thread reference map, contains a TVar
                  (TVar (ThreadCompl u),     -- where thread completion result is stored,
                  (MVar ()))                 -- keyed by the GHC thread identifier from forkIO.
 ,parTVar :: TVar (ThreadCompl u)            -- parent's TVar to send notifications to.
 ,currScope :: Scope                         -- current scope.
}

-- | A structure to store thread completion result. A thread may complete with
-- its state returned to the parent or hidden; send the state to the parent and
-- continue, send the state and detach, die, or die hard.

data ThreadCompl u =
   ThreadStarted                             -- ^ Thread just started: this value is initially
                                             -- stored in the TVar that holds thread user state.
 | ThreadCompleted u                         -- ^ Thread completed normally, and its state is 
                                             -- provided to its parent. This returned
                                             -- state may be merged into the parent state. This
                                             -- approach replaces the Plan 9 approach allowing
                                             -- child processes to share their system state
                                             -- (namespace, environment, etc).
 | ThreadDetached (Maybe u)                  -- ^ Thread detached from its parent (that is, became
                                             -- a leader of its own group). It may send a snapshot
                                             -- of its state to the parent.
 | ThreadRunning u                           -- ^ Thread is running and notifies the parent
                                             -- of its state. This may be used in interactive
                                             -- programs when a child process (e. g. a shell) wants
                                             -- to update its parent (an application manager)
                                             -- of its namespace change. Requires some cooperation
                                             -- between parent and child.
 | ThreadDied String                         -- ^ Uncaught exception (including asynchronous)
                                             -- was processed at the top level within the NineM
                                             -- monad. Thread state is obviously lost. The string
                                             -- contains some description of the exception caught.
 | ThreadDiedHard String                     -- ^ Uncaught exception was processed at the IO monad
                                             -- level (that is, uncaught within NineM).
   deriving (Show)

-- The NineM monad. It is parameterized by the type of user part of the state.
-- At the NineM level, no operations over the internal structure of user state
-- are performed, but thread management functions allow for user state exchange
-- between parent and child threads.

type NineM u a = StateT (ThreadState u) IO a

-- The NameSpaceM monad is based on the StateT monad transformer on top
-- of the NineM monad. It operates at thread level and encapsulates namespaced
-- I/O operations. Namespace structures are provided to NineM as user state type.
-- Forked thread inherit parent's namespace, and upon completion or notification
-- the parent may incorporate namespace changes done by child threads.

type NameSpaceM a = StateT NameSpace (StateT (ThreadState NameSpace) IO) a

-- The Scope data structure. It contains a link to the parent scope, a map of
-- devices indexed by device letter and tree, and a set of DEVFID pairs.
-- It is maintained throughout the thread lifetime, that only one device
-- is attached per letter-tree pair. FIDs is what is tracked per scope.
-- A function that runs inside scope may allocate some FIDs in open devices.

data Scope = Scope {
  pScope :: Maybe Scope                     -- parent scope
 ,devLT :: M.Map (Char, FilePath) Device9P  -- letter-tree device map
 ,fidSet :: S.Set (DEVFID)                  -- set of active DEVFIDs
} deriving (Show)

