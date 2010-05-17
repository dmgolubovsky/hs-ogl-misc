------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLFWApp.Events
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  depends on external libraries
-- 
--
--
-- Data Types for GLFW Events
------------------------------------------------------------------

module Graphics.UI.GLFWApp.Events (
  GLFWEvent (..)
) where

import HS_AUTOGLFW_H
import Data.Char
import Control.Concurrent

data GLFWEvent =
   WindowRefresh
 | WindowClose
 | WindowSize Int Int
 | KeyEvt Int Int
 | CharEvt Char Int
 | MouseButton Int Int
 | MouseWheel Int
 | MousePos Int Int
  deriving (Eq, Show)

-- Callback wrappers.

rfcb ch = forkIO (writeChan ch WindowRefresh) >> return ()
clcb ch = forkIO (writeChan ch WindowClose) >> return (fromIntegral c_GL_TRUE)
szcb ch w h = forkIO (writeChan ch $ WindowSize (fromIntegral w) (fromIntegral h)) >> return ()
chcb ch c a = forkIO (writeChan ch $ CharEvt (chr $ fromIntegral c) (fromIntegral a)) >> return ()
kycb ch k a = forkIO (writeChan ch $ KeyEvt (fromIntegral k) (fromIntegral a)) >> return ()
mbcb ch b a = forkIO (writeChan ch $ MouseButton (fromIntegral b) (fromIntegral a)) >> return ()
mpcb ch x y = forkIO (writeChan ch $ MousePos (fromIntegral x) (fromIntegral y)) >> return ()
mwcb ch p = forkIO (writeChan ch $ MouseWheel (fromIntegral p)) >> return ()


