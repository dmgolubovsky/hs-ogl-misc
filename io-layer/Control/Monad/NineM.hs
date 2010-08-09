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
) where

import Prelude hiding (catch)
import System.IO9.Device hiding (get, put)
import Control.Monad
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.Char
import qualified Data.IntMap as I
import qualified Data.Map as M

-- The NineM monad transformer is based on the StateT transformer. It operates at thread
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
 ,thrMap :: M.Map ThreadId                   -- Child thread reference map, contains a MVar
                  (MVar (ThreadCompl u))     -- where thread completion result is stored,
                                             -- keyed by the GHC thread identifier from forkIO.
}

-- | A structure to store thread completion result. A thread may complete with
-- its state returned to the parent or hidden; send the state to the parent and
-- continue, send the state and detach, die, or die hard.

data ThreadCompl u =
   ThreadCompleted u                         -- ^ Thread completed normally, and its state is 
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

initState :: u -> ThreadState u

initState u = ThreadState 0 
                          I.empty 
                          M.empty
                          M.empty
                          u
                          M.empty

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
-- threads known. The thread is associated with some MVar where the completion
-- code will be stored (see 'ThreadCompl' data definition).

threadLife :: u -> MVar (ThreadCompl u) -> NineM u () -> IO ()

threadLife u mv x = thrIO `catch` \(e :: SomeException) ->
  putMVar mv (ThreadDiedHard $ show e) >> return () where
  thrIO = flip evalStateT (initState u) (thrNine `catchSome` \(ee :: SomeException) -> do
    liftIO $ putMVar mv $ ThreadDied $ show ee 
    cleanUp)
  thrNine = x >> get >>= liftIO . putMVar mv . ThreadCompleted . userState >> cleanUp

-- Utility: catch SomeException inside a StateT transformer.
    
m `catchSome` h = StateT $ \s -> runStateT m s 
   `catch`  \(e :: SomeException) -> runStateT (h e) s

-- Utility: clean up the thread state (user state remains untouched).

cleanUp :: NineM u ()

cleanUp = return ()

-- | Run the "main" program. This is the only entry point into this monad visible
-- from the outside. 


startup :: Show u => u -> NineM u () -> IO ()

startup u x = do
  mv <- newEmptyMVar
  threadLife u mv x
  readMVar mv >>= putStrLn . show

{-

startup u x = flip evalStateT (initState u) ((x `catchSome` \ex ->
  liftIO (putStrLn ("NineM Caught: " ++ show ex)) >> return ()) >> liftIO (putStrLn "Bye"))

-}


