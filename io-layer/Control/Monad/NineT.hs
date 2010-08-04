------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.NineT
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A monad transformer on top of IO to communicate with virtual devices
------------------------------------------------------------------

module Control.Monad.NineT (
  Device
 ,device
 ,freshdev
) where

import System.IO9.Device hiding (get, put)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Data.Char
import qualified Data.IntMap as I
import qualified Data.Map as M

-- The NineT monad transformer is based on the StateT transformer. It operates at thread
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
 ,devMap :: I.IntMap Device9P           -- Device reference map. Each time a device is attached,
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

type NineT a = StateT ThreadState IO a

-- Utility: get a unique (thread-wise) integer number.

nextInt :: NineT Int

nextInt = do
  s <- get
  let x = intGen s + 1
  put s {intGen = x}
  return x

-- Utility: update devices map with new state of a device by given index.

updDev :: Int -> Device9P -> NineT ()

updDev di dev = do
  s <- get
  let dm' = I.insert di dev (devMap s)
  put s {devMap = dm'}
  return ()

-- | Register a device by letter adding it into the device table.                  
-- Fetching a device from the table always gives a fresh interface on which
-- even the Version message was not tried.

device :: Char                         -- ^ Device character       
       -> IO Device9P                  -- ^ Driver creation function
       -> NineT ()

device dc di = do
  s <- get
  d <- liftIO di
  let dt' = M.insert dc d (devTab s)
  put s {devTab = dt'}
  return ()

-- | Find a device by letter. This function always returns a "fresh" instance of a device,
-- just as it was registered by 'device'. The function fails if no device can be
-- found by the letter provided.

freshdev :: Char -> NineT Device

freshdev dc = do
  mbd <- get >>= return . M.lookup dc . devTab
  case mbd of
    Nothing -> fail $ "no device driver found by letter " ++ [dc]
    Just dev -> do
      di <- nextInt
      updDev di dev
      return $ Device di

{-

device size vers dev = do
  tag <- nextInt >>= return . fromIntegral
  resp <- liftIO $ dev $ Msg TTversion tag (Tversion (fromIntegral size) vers)
  case re_msg resp of
    Msg TRerror _ (Rerror err) -> fail err
    Msg TRversion _ _ -> do
      dev <- nextInt
      updDev dev (re_cont resp)
      return $ Device dev
    z -> fail $ "invalid response from device to version msg: " ++ show z

-}
