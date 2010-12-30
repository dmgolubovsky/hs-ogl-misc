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
-- User-induced events
------------------------------------------------------------------

module Graphics.UI.GLFWApp.Events (
  GLFWEvent (..)
 ,GLFWApp (..)
 ,GLFWAppData (..)
 ,defaultApp
 ,glfwMain
 ,winSize
 ,winRect
 ,glMousePos
 ,glMousePosRatio
) where

import HS_AUTOGLFW_H
import Data.Char
import Data.Maybe
import System.Exit
import Data.Shapes2D
import Control.Monad
import Control.Concurrent
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
-- Note that the default redrawProc calls gluOrtho2D 0 x 0 y where x and y are window
-- dimensions in pixels after resize. This works well with 'glMousePos'

class GLFWAppData app where
  eventHandler :: app -> GLFWEvent -> IO app
  eventHandler app e = return app
  needRedraw :: app -> Bool
  needRedraw app = False
  redrawProc :: app -> Int -> Int -> IO ()
  redrawProc app x y = return ()
  doneRedraw :: app -> app
  doneRedraw = id
  reshapeProc :: app -> Int -> Int  -> IO ()
  reshapeProc = const defaultReshape
  initProc :: app -> IO ()
  initProc = const defaultInit
  postedEvents :: app -> [GLFWEvent]
  postedEvents = const []

instance GLFWAppData () -- Dummy instance for a default application.

-- | Data type to describe basic parameters of a GLFW application.

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

-- | Get the current mouse position in OpenGL integer coordinates (window pixels).

glMousePos :: IO (Int, Int)

glMousePos =
  alloca $ \pw -> alloca $ \ph -> do
    f_glfwGetMousePos pw ph
    x <- peek pw >>= return . fromIntegral
    y <- peek ph >>= return . fromIntegral
    (w, h) <- winSize
    return (x, h - y)

-- | Get the current mouse position ratios to window dimensions.

glMousePosRatio :: IO (Double, Double)

glMousePosRatio =
  alloca $ \pw -> alloca $ \ph -> alloca $ \mx -> alloca $ \my -> do
    f_glfwGetWindowSize pw ph
    f_glfwGetMousePos mx my
    w <- peek pw >>= return . fromIntegral
    h <- peek ph >>= return . fromIntegral
    x <- peek mx >>= return . fromIntegral
    y <- peek my >>= return . fromIntegral
    let y' = h - y
    return (x / w, y' / h)

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
  mainLoop ch gst (winWidth app) (winHeight app) where
    mainLoop ch gst wx wy = do
      ec <- isEmptyChan ch
      case ec of
        True -> f_glfwWaitEvents >> mainLoop ch gst wx wy
        False -> do
          (f, e, nx, ny) <- readChan ch >>= \e' -> case e' of
            WindowSize x y -> reshapeProc gst x y >> return (False, e', x, y)
            WindowRefresh -> return (True, e', wx, wy)
            WindowClose -> do
              eventHandler gst WindowClose
              f_glfwTerminate
              exitWith ExitSuccess
            _ -> return (False, e', wx, wy)
          gst' <- eventHandler gst e
          mapM_ (writeChan ch) (postedEvents gst')
          case f || needRedraw gst' of
            True -> do
              redrawProc gst' nx ny
              f_glfwSwapBuffers
              mainLoop ch (doneRedraw gst') nx ny
            False -> mainLoop ch gst' nx ny
          

                        

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

