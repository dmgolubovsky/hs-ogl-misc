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
 ,DEVFID
 ,ScopeR (..)
 ,enterScope
 ,exitScope
 ,runScope
 ,noDevice
 ,getdev
 ,walkdev
 ,statfid
 ,openfid
 ,readdir
 ,readfid
 ,devmsg
 ,nextInt
 ,startup
 ,spawn
 ,wait
 ,catchSome
) where

import PrivateDefs
import Prelude hiding (catch)
import System.FilePath
import System.IO9.Error
import System.IO9.Device hiding (get, put)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.Int
import Data.Word
import Data.Char
import Data.Maybe
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Control.Exception as E
import qualified System.IO9.Device as D

-- The NineM monad is based on the StateT transformer. It operates at thread
-- level, and provides convenient wrappers for 9P2000 operations as well as functions
-- for thread management. Not much is exported from this module; most of data types remains
-- opaque to this monad's "clients".

-- | No-device.

noDevice = Device (-1)

-- | A class for values returned from a scope. These values must somehow refer to
-- zero or more device-FID pairs. These pairs will be retained after the scope is exited,
-- and added to the parent scope. By default, a value does not refer to anything.

class ScopeR a where
  retains :: a -> [DEVFID]
  retains _ = []

instance (ScopeR a) => ScopeR [a] where
  retains as = concatMap retains as

instance ScopeR ()

instance ScopeR DEVFID where
  retains a = [a]

instance ScopeR Stat

-- | Enter a new scope. Thie function updates the scope reference in the thread state
-- by creating a new empty scope and linking it to the current scope which becomes a parent.

enterScope :: NineM u ()

enterScope = do
  s <- get
  let n = Scope (Just $ currScope s) S.empty
  put s {currScope = n}

-- | Exit the scope and return whatever is retained to the parent scope, clunk
-- anything else.

exitScope :: [DEVFID] -> NineM u ()

exitScope rtns = do
  s <- get
  let c = currScope s
      r = S.fromList rtns
      clunks = S.toList (fidSet c `S.difference` r)
  forM_ clunks $ \(dev, fid) -> devmsg dev (Tclunk fid)
  ss <- get
  case pScope c of
    Nothing -> return ()
    Just p -> put ss {currScope = p {fidSet = fidSet p `S.union` r}}

-- | Run a device I/O action in a scope. Whatever is returned, retains some or none 
-- DEVFIDs for the parent scope.

runScope :: (ScopeR r) => NineM u r -> NineM u r

runScope x = do
  enterScope
  xr <- x `catchSome` (\e -> exitScope [] >> E.throw e)
  exitScope (retains xr)
  return xr
 
-- Initialize thread state.

initState :: u -> TVar (ThreadCompl u) -> ThreadState u

initState u tv = ThreadState 0 
                             I.empty 
                             M.empty
                             M.empty
                             u
                             M.empty
                             tv
                             M.empty
                             (Scope Nothing S.empty)

-- | Get a unique (thread-wise) integer number.

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
    Nothing -> throw $ OtherError $ "internal: cannot find device " ++ show di
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
    Nothing -> throw Ebadsharp
    Just dev -> do
      di <- nextInt
      s <- get
      let dm' = I.insert di (dev, dc) (devMap s)
      put s {devMap = dm'}
      return $ Device di


-- | Get a device for the given letter and tree. If such a device had been attached before,
-- use it. Otherwise allocate a fresh device, execute the optional authentication code,
-- and return it. This function operates in a scope and retains a DEVFID for its parent scope.
-- The FID for the root of device's tree though, is not added to the current scope and thus 
-- preserved from being clunked. It is saved in 'devLT' member of the thread state.
-- The authentication function has to perform the complete exchange with the device
-- to authorize access to the given root of the selected tree. It is expected to return
-- a FID that belongs to the device root. This FID will be dropped from the current
-- scope's 'fidSet' even if it was added to it to become permanent through the remainder
-- of the thread lifetime.

getdev :: Char -> FilePath -> (Device -> FilePath -> NineM u FID) -> NineM u DEVFID

getdev dc "" auth = getdev dc "/" auth

getdev dc fp auth = runScope $ do
  mbd <- get >>= return . M.lookup (dc, fp) . devLT
  (rd, rf) <- case mbd of
                Just df -> return df
                Nothing -> do
                  d <- freshdev dc
                  f <- auth d fp
                  s <- get
                  let c = currScope s
                      fid = (d, f)
                  put s {currScope = c {fidSet = S.delete fid (fidSet c)}
                        ,devLT = M.insert (dc, fp) fid (devLT s)}
                  return fid
  nf <- nextInt >>= return . fromIntegral
  devmsg rd $ Twalk rf nf []
  return (rd, nf)

-- | Walk a device from the given DEVFID to the given split path. The function
-- fails if the driver returns an error message as well as if the walk is incomplete.
-- FID for the destination path is returned. The destination path is considered relative
-- to the one source DEVFID is for. QIDs returned by the driver are lost, only their number
-- is counted.

walkdev :: DEVFID -> [FilePath] -> NineM u DEVFID

walkdev (dev, ffid) fps = runScope $ nextInt >>= return . fromIntegral >>= \tfid -> do
  (Rwalk rwlk) <- devmsg dev $ Twalk ffid tfid fps
  case (length rwlk, length fps) of
    (1, 0) -> return (dev, tfid)
    (x, y) | x == y -> return (dev, tfid)
    _ -> throw Enonexist

-- | Obtain a 'Stat' structure for a given device/fid.

statfid :: DEVFID -> NineM u Stat

statfid (dev, fid) = do
   (Rstat r) <- devmsg dev $ Tstat fid
   return $ head r                -- NB to be changed once the NineP module is updated.
    

-- | Open the given FID on the given device. IOUNIT is ignored for the local message
-- exchange, but Qid is valuable.

openfid :: DEVFID -> Word8 -> NineM u Qid

openfid (dev, fid) mode = do
  (Ropen q i) <- devmsg dev $ Topen fid mode
  return q

-- | Read contents of a file as a 'ByteString'. The FID should be open before 'readfid'
-- may be used on it.

readfid :: DEVFID -> Word64 -> Word32 -> NineM u B.ByteString

readfid (dev, fid) off cnt = do
  (Rread r) <- devmsg dev $ Tread fid off cnt
  return r


-- | Read contents of a directory as a list of 'Stat' structures. This function sends a
-- read message using given device and FID. The returned 'ByteString' will be parsed as
-- a sequence of encoded status structures. If decoding fails, whatever was decoded is returned.
-- The FID should be open before 'readdir' can be used on it.
 
readdir :: DEVFID -> NineM u [Stat]

readdir (dev, fid) = do
  (Rread r) <- devmsg dev $ Tread fid 0 9999
  if B.null r 
    then return []
    else return $ runGet (many D.get) r

many :: Get a -> Get [a]

many prs = many' [] where 
  many' a = do
    s <- prs
    r <- isEmpty
    case r of
      True -> return (reverse a)
      False -> many' (s:a)

  

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
    Nothing -> throw $ OtherError $ "internal: cannot find device " ++ show di
    Just (d, _) -> do
      resp <- liftIO $ d msg
      updDev di $ re_cont resp
      let rspb = msg_body $ re_msg resp
      case rspb of
        Rerror e -> throw $ OtherError $ "internal: 9P error " ++ e
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

-- | Catch SomeException inside a StateT transformer.

catchSome :: StateT s IO a -> (SomeException -> StateT s IO a) -> StateT s IO a
    
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
  let devs = I.keys $ devMap s
      thrs = M.keys $ thrMap s
  mapM_ (flip devmsg (Tclunk c_NOFID) . Device) devs
  liftIO $ mapM_ killThread thrs
  

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
-- thread. User state will also be inherited. The spawn function may only be called from
-- the toplevel scope.

spawn :: NineM u () -> NineM u ThreadId

spawn thr = do
  s <- get
  when (isJust $ pScope (currScope s)) $ 
    throw $ OtherError "spawn: called from a nested scope"
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
    Nothing -> throw Enochild
    Just (tv, mv) -> liftIO $ do
      readMVar mv
      atomically $ readTVar tv

