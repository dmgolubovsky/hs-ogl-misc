------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.NineM
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A monad on top of IO to communicate with virtual devices
------------------------------------------------------------------

module Control.Monad.NineM (
  Device
 ,ThreadCompl (..)
 ,device
 ,freshdev
 ,devmsg
 ,startup
 ,spawn
 ,wait
) where

import Prelude hiding (catch)
import System.IO9.Device hiding (get, put)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.Char
import Data.Maybe
import qualified Data.IntMap as I
import qualified Data.Map as M

-- The NineM monad is based on the StateT transformer. It operates at thread
-- level, and provides convenient wrappers for 9P2000 operations as well as functions
-- for thread management. Not much is exported from this module; most of data types remains
-- opaque to this monad's "clients".

-- | A newtype wrapper for a device reference (just a number indeed). It is opaque
-- to the external code that uses this monad.

newtype Device = Device {devRef :: Int}

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

-- Initialize thread state.

initState :: u -> TVar (ThreadCompl u) -> ThreadState u

initState u tv = ThreadState 0 
                             I.empty 
                             M.empty
                             M.empty
                             u
                             M.empty
                             tv

-- The monad itself. It is parameterized by the type of user part of the state.
-- At the NineM level, no operations over the internal structure of user state
-- are performed, but thread management functions allow for user state exchange
-- between parent and child threads.

type NineM u a = StateT (ThreadState u) IO a

-- Utility: get a unique (thread-wise) integer number.

nextInt :: NineM u Int

nextInt = do
  s <- get
  let x = intGen s + 1
  put s {intGen = x}
  return x

-- Utility: get device by device index.

findDev :: Int -> NineM u (Maybe (Device9P, Char))

findDev di = get >>= return . I.lookup di . devMap

-- Utility: update devices map with new state of a device by given index.

updDev :: Int -> Device9P -> NineM u ()

updDev di dev = do
  s <- get
  mbd <- findDev di
  case mbd of
    Nothing -> fail "invalid device index"
    Just (_, dc) -> do
      let dm' = I.insert di (dev, dc) (devMap s)
      put s {devMap = dm'}
      return ()

-- | Register a device by letter adding it into the device table.                  
-- Fetching a device from the table always gives a fresh interface on which
-- even the Version message was not tried.

device :: Char                         -- ^ Device character       
       -> IO Device9P                  -- ^ Driver creation function
       -> NineM u ()

device dc di = do
  s <- get
  d <- liftIO di
  let dt' = M.insert dc d (devTab s)
  put s {devTab = dt'}
  return ()

-- | Find a device by letter. This function always returns a "fresh" instance of a device,
-- just as it was registered by 'device'. The function fails if no device can be
-- found by the letter provided.

freshdev :: Char -> NineM u Device

freshdev dc = do
  mbd <- get >>= return . M.lookup dc . devTab
  case mbd of
    Nothing -> fail $ "no device driver found by letter " ++ [dc]
    Just dev -> do
      di <- nextInt
      s <- get
      let dm' = I.insert di (dev, dc) (devMap s)
      put s {devMap = dm'}
      return $ Device di

-- | Send a 9P2000 message to the given device. The device state in devmap is always updated,
-- even when an error response is returned. In case of a error response, the function fails
-- with the string provided with the error message. Otherwise response is returned.

devmsg :: Device -> VarMsg -> NineM u VarMsg

devmsg (Device di) msgb = do
  tag <- nextInt >>= return . fromIntegral
  let msg = Msg {
        msg_typ = body2type msgb
       ,msg_tag = tag
       ,msg_body = msgb}
  mbd <- findDev di
  case mbd of
    Nothing -> fail "invalid device number"
    Just (d, _) -> do
      resp <- liftIO $ d msg
      updDev di $ re_cont resp
      let rspb = msg_body $ re_msg resp
      case rspb of
        Rerror e -> fail e
        _ -> return rspb

-- Thread life cycle. Being started with some initial state, it proceeds
-- in the NineM monad until it returns, or an exception is caught. Once
-- the thread terminates, its state cleanup is performed which involves
-- clunking of all devices currently attached, and killing of all child
-- threads known. The thread is associated with some TVar where the completion
-- code will be stored (see 'ThreadCompl' data definition).

threadLife :: Maybe (ThreadState u) -> u -> TVar (ThreadCompl u) -> MVar () -> NineM u () -> IO ()

threadLife mbsu u tv mv x = (thrIO `catch` \(e :: SomeException) ->
  atomically (writeTVar tv . ThreadDiedHard $ show e) >> return ()) >> putMVar mv () where
  thrs = fromMaybe (initState u tv) mbsu
  thrIO = flip evalStateT thrs 
                          (thrNine `catchSome` \(ee :: SomeException) -> do
                             cleanUp
                             notifyParent tv . ThreadDied $ show ee)
  thrNine = x >> cleanUp >> get >>= notifyParent tv . ThreadCompleted . userState

-- Utility: modify state stored in the parent's TVar. Use a non-blocking function here
-- since the parent may not want to read its children state immediately. Unread child states
-- are lost.

notifyParent :: TVar (ThreadCompl u) -> ThreadCompl u -> NineM u ()

notifyParent tv tc = liftIO $ atomically (writeTVar tv tc) >> return ()  

-- Utility: catch SomeException inside a StateT transformer.
    
m `catchSome` h = StateT $ \s -> runStateT m s 
   `catch`  \(e :: SomeException) -> runStateT (h e) s

-- Utility: clean up the thread state (user state remains untouched).
-- Things to clean up are:
--  - clunk all attached (listed in devMap) devices with c_NOFID
--  - kill all child threads (killThread will be sent to all of them; per GHC specs
--    if a thread has finished, killThread has no effect).
-- It is expected that each child thread when killed, will cleanup its own state
-- (provided that each thread is started with threadLife).
-- If an exception raises during cleanUp, it will be caught outside of the NineM,
-- so ThreadDiedHard will be reported.

cleanUp :: NineM u ()

cleanUp = do
  s <- get
  mapM_ (flip devmsg (Tclunk c_NOFID) . Device) (I.keys $ devMap s)
  liftIO $ mapM_ killThread (M.keys $ thrMap s)
  

-- | Run the "main" program. This is the only entry point into this monad visible
-- from the outside. 


startup :: Show u => u -> NineM u () -> IO ()

startup u x = do
  mv <- newEmptyMVar
  tv <- atomically $ newTVar ThreadStarted
  threadLife Nothing u tv mv x
  atomically (readTVar tv) >>= putStrLn . show

-- | Spawn a new thread given the entry point. The thread will inherit its parent's state.
-- Devices will remain attached, however device drivers may reject messages from the child
-- thread. User state will also be inherited.

spawn :: NineM u () -> NineM u ThreadId

spawn thr = do
  s <- get
  tv <- liftIO . atomically $ newTVar ThreadStarted
  mv <- liftIO newEmptyMVar
  let s' = s {thrMap = M.empty
             ,parTVar = tv}
  t <- liftIO . forkIO $ threadLife (Just s') undefined tv mv thr
  let tm' = M.insert t (tv, mv) (thrMap s)
  put s {thrMap = tm'}
  return t

-- | Wait for child thread completion or detach. Return the thread completion result. 
-- This function blocks on the MVar found in the child threads map and upon wakeup
-- reads the value from the TVar. Since MVar is written after TVar is modified with
-- thread completion result, consistent value will be returned.

wait :: ThreadId -> NineM u (ThreadCompl u)

wait thr = do
  s <- get
  let thrv = M.lookup thr (thrMap s)
  case thrv of
    Nothing -> fail $ "not a child thread: " ++ show thr
    Just (tv, mv) -> liftIO $ do
      readMVar mv
      atomically $ readTVar tv

