------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLFWApp
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  depends on external libraries
-- 
--
--
-- General Support of GLFW-based Applications        
------------------------------------------------------------------

module Graphics.UI.GLFWApp (
  module HS_AUTOGLFW_H
 ,module Graphics.UI.GLFWApp.Texture
 ,GLFWEvent (..)
 ,GLFWApp (..)
 ,GLFWAppData (..)
 ,defaultApp
 ,glfwMain
 ,winSize
 ,winRect
 ,glMousePos
) where

import HS_AUTOGLFW_H
import Data.Char
import Data.Maybe
import System.Exit
import Data.Shapes2D
import Control.Monad
import Control.Concurrent
import Graphics.UI.GLFWApp.Texture
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as C

-- | Data type to describe a GLFW input event as it is passed to 
-- an application's event handling function.

data GLFWEvent =
   WindowRefresh                   -- ^ Window got uncovered/moved, and needs to be redrawn
 | WindowClose                     -- ^ Window is about to be closed: terminate the application
 | WindowSize Int Int              -- ^ Window size changed to the new size (x and y in pixels)
 | KeyEvt Int Int                  -- ^ A key was pressed: key code and flag press/release
 | CharEvt Char Int                -- ^ Character input: char (Unicode) value and flag press/rel
 | MouseButton Int Int             -- ^ Mouse button was pressed: button number and flag press/rel
 | MouseWheel Int                  -- ^ Mouse wheel was turned: positive or negative position
 | MousePos Int Int                -- ^ Mouse move: position (x, y in mouse coordinates, not OGL!!!)
  deriving (Eq, Show)

-- | Type class for a GLFW application custom data. Methods are provided for event
-- handling, and for initializing/updating/redrawing of the window contents.

class GLFWAppData gst where
  eventHandler :: gst -> GLFWEvent -> IO gst
  eventHandler gst e = return gst
  needRedraw :: gst -> Bool
  needRedraw gst = False
  redrawProc :: gst -> IO ()
  redrawProc gst = return ()
  reshapeProc :: gst -> Int -> Int  -> IO ()
  reshapeProc = const defaultReshape
  initProc :: gst -> IO ()
  initProc = const defaultInit
  postedEvents :: gst -> [GLFWEvent]
  postedEvents = const []

instance GLFWAppData () -- Dummy instance for a default application.

-- | Data type to describe basic parameters of a GLFW application. It is parameterized
-- by user-defined state type (gst).

data GLFWApp = GLFWApp {
   winWidth :: Int                 -- ^ Application window initial width in pixels
  ,winHeight :: Int                -- ^ Application window initial height in pixels
  ,redBits :: Int                  -- ^ Red color depth in bits
  ,greenBits :: Int                -- ^ Green color depth in bits
  ,blueBits :: Int                 -- ^ Blue  color depth in bits
  ,alphaBits :: Int                -- ^ Alpha channel depth in bits
  ,depthBits :: Int                -- ^ Depth (z-coordinate) buffer bits
  ,stencilBits :: Int              -- ^ Stencil buffer bits
  ,winMode :: Int                  -- ^ Application mode (windowed/fullscreen)
  ,appTitle :: String              -- ^ Application title
}

-- | Default parameters of an application

defaultApp :: GLFWApp

defaultApp = GLFWApp {
   winWidth = 400
  ,winHeight = 400
  ,redBits = 0
  ,greenBits = 0
  ,blueBits = 0
  ,alphaBits = 0
  ,depthBits = 16
  ,stencilBits = 0
  ,appTitle = "Default GLFW application"
  ,winMode = fromIntegral c_GLFW_WINDOW}

-- | Get the current application window size in pixels (width, height).

winSize :: IO (Int, Int)

winSize =
  alloca $ \pw -> alloca $ \ph -> do
    f_glfwGetWindowSize pw ph
    w <- peek pw >>= return . fromIntegral
    h <- peek ph >>= return . fromIntegral
    return (w, h)

-- | Get the rectangle of the current application window.

winRect :: IO Rect

winRect = do
  (w, h) <- winSize
  return Rect {rectOrig = Point 0 0, rectWdt = w, rectHgt = h}

-- | Get the current mouse position in OpenGL integer coordinates.

glMousePos :: IO (Int, Int)

glMousePos =
  alloca $ \pw -> alloca $ \ph -> do
    f_glfwGetMousePos pw ph
    x <- peek pw >>= return . fromIntegral
    y <- peek ph >>= return . fromIntegral
    (w, h) <- winSize
    return (x, h - y)

-- | Main function for a GLFW application. It is to be called with the initial
-- user-specified application state.

glfwMain :: (GLFWAppData gst) => GLFWApp -> gst -> IO ()

glfwMain app gst = do
  f_glfwInit
  t <- f_glfwOpenWindow (fromIntegral $ winWidth app)
                        (fromIntegral $ winHeight app)
                        (fromIntegral $ redBits app)
                        (fromIntegral $ greenBits app)
                        (fromIntegral $ blueBits app)
                        (fromIntegral $ alphaBits app)
                        (fromIntegral $ depthBits app)
                        (fromIntegral $ stencilBits app)
                        (fromIntegral $ winMode app)
  when (t == fromIntegral c_GL_FALSE) $ do
    f_glfwTerminate
    exitWith (ExitFailure 1)
  withBString (appTitle app) f_glfwSetWindowTitle
  ch <- newChan
  w_glfwSetWindowCloseCallback_1 (clcb ch) >>= f_glfwSetWindowCloseCallback
  w_glfwSetWindowSizeCallback_1 (szcb ch) >>= f_glfwSetWindowSizeCallback
  w_glfwSetWindowRefreshCallback_1 (rfcb ch) >>= f_glfwSetWindowRefreshCallback
  w_glfwSetCharCallback_1 (chcb ch) >>= f_glfwSetCharCallback
  w_glfwSetKeyCallback_1 (kycb ch) >>= f_glfwSetKeyCallback
  w_glfwSetMouseButtonCallback_1 (mbcb ch) >>= f_glfwSetMouseButtonCallback
  w_glfwSetMouseWheelCallback_1 (mwcb ch) >>= f_glfwSetMouseWheelCallback
  w_glfwSetMousePosCallback_1 (mpcb ch) >>= f_glfwSetMousePosCallback
  initProc gst
  writeChan ch WindowRefresh
  mainLoop ch gst where
    mainLoop ch gst = do
      ec <- isEmptyChan ch
      case ec of
        True -> f_glfwWaitEvents >> mainLoop ch gst
        False -> do
          (f, e) <- readChan ch >>= \e' -> case e' of
            WindowSize x y -> reshapeProc gst x y >> return (False, e')
            WindowRefresh -> return (True, e')
            WindowClose -> do
              eventHandler gst WindowClose
              f_glfwTerminate
              exitWith ExitSuccess
            _ -> return (False, e')
          gst' <- eventHandler gst e
          mapM_ (writeChan ch) (postedEvents gst')
          when (f || needRedraw gst') $ do
            redrawProc gst'
            f_glfwSwapBuffers
          mainLoop ch gst'
          

                        

-- Callback wrappers (not exported).

rfcb ch = forkIO (writeChan ch WindowRefresh) >> return ()
clcb ch = forkIO (writeChan ch WindowClose) >> return (fromIntegral c_GL_TRUE)
szcb ch w h = forkIO (writeChan ch $ WindowSize (fromIntegral w) (fromIntegral h)) >> return ()
chcb ch c a = forkIO (writeChan ch $ CharEvt (chr $ fromIntegral c) (fromIntegral a)) >> return ()
kycb ch k a = forkIO (writeChan ch $ KeyEvt (fromIntegral k) (fromIntegral a)) >> return ()
mbcb ch b a = forkIO (writeChan ch $ MouseButton (fromIntegral b) (fromIntegral a)) >> return ()
mpcb ch x y = forkIO (writeChan ch $ MousePos (fromIntegral x) (fromIntegral y)) >> return ()
mwcb ch p = forkIO (writeChan ch $ MouseWheel (fromIntegral p)) >> return ()

-- Default functions for an application (not exported).

defaultReshape x y = do
  f_glViewport 0  0 (fromIntegral x) (fromIntegral y)
  f_glMatrixMode (fromIntegral c_GL_PROJECTION)
  f_glLoadIdentity
  f_gluOrtho2D 0.0 (realToFrac x)  0.0 (realToFrac y)
  return ()

defaultInit = do
  f_glClearColor 0 0 0 0
  f_glShadeModel (fromIntegral c_GL_FLAT)
  return ()

defaultHandler a e = return (a, Nothing)

-- Same as withCString, but Unicode is supported (NB: glfw itself uses 
-- X11 function to set window title which is not unicode capable).

withBString :: String -> (CString -> IO a) -> IO a

withBString s a = B.useAsCString (C.fromString s) a

