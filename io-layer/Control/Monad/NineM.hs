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
 ,device
 ,freshdev
 ,devmsg
 ,startup
) where

import Prelude hiding (catch)
import System.IO9.Device hiding (get, put)
import Control.Monad
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
-- for the code that uses this monad.

newtype Device = Device {devRef :: Int}

-- Internal structure of a thread state.

data ThreadState = ThreadState {
  intGen :: Int                         -- Integer number incremental generator
                                        -- to use for tags, fids, device indices, etc.
 ,devMap :: I.IntMap (Device9P, Char)   -- Device reference map. Each time a device is attached,
                                        -- a new index is generated, and the device is
                                        -- referenced by that index through this map.
 ,thrEnv :: M.Map String String         -- Thread's own environment needed at this level.
 ,devTab :: M.Map Char Device9P         -- Table of devices (by letter)

}

-- Initialize thread state.

initState = ThreadState 0 
                        I.empty 
                        M.empty
                        M.empty

-- The transformer itself.

type NineM a = StateT ThreadState IO a

-- Utility: get a unique (thread-wise) integer number.

nextInt :: NineM Int

nextInt = do
  s <- get
  let x = intGen s + 1
  put s {intGen = x}
  return x

-- Utility: get device by device index.

findDev :: Int -> NineM (Maybe (Device9P, Char))

findDev di = get >>= return . I.lookup di . devMap

-- Utility: update devices map with new state of a device by given index.

updDev :: Int -> Device9P -> NineM ()

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
       -> NineM ()

device dc di = do
  s <- get
  d <- liftIO di
  let dt' = M.insert dc d (devTab s)
  put s {devTab = dt'}
  return ()

-- | Find a device by letter. This function always returns a "fresh" instance of a device,
-- just as it was registered by 'device'. The function fails if no device can be
-- found by the letter provided.

freshdev :: Char -> NineM Device

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

devmsg :: Device -> VarMsg -> NineM VarMsg

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

-- | Run the "main" program. This is the only entry point into this monad visible
-- from the outside. 

instance Error AsyncException

startup :: NineM () -> IO ()

startup x = flip evalStateT initState ((x `catchSome` \ex ->
  liftIO (putStrLn ("NineM Caught: " ++ show ex)) >> return ()) >> liftIO (putStrLn "Bye"))

m `catchAsync` h = StateT $ \s -> runStateT m s 
   `catch`  \(e :: AsyncException) -> runStateT (h e) s

m `catchSome` h = StateT $ \s -> runStateT m s 
   `catch`  \(e :: SomeException) -> runStateT (h e) s



