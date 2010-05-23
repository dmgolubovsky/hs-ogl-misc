--
-- OpenGL image viewer, based upon stb-image example.
--

module Main where

import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Data.Maybe
import Data.Shapes2D

import Graphics.UI.GLFWApp

data VWR = VWR {
  fp :: FilePath
 ,texture :: Maybe GLTexture
 ,nredraw :: Bool
}

instance GLFWAppData VWR where
  needRedraw = nredraw
  redrawProc = redraw
  eventHandler = evt

 
main = do
  args <- getArgs

  fname <- case args of
    (fn:_) -> return fn
    _    -> putStrLn "usage: glviewer <imagefile.ext>" >> exitFailure

  glfwMain defaultApp {appTitle = fname
                      ,winWidth = 400
                      ,winHeight = 400} 
           VWR {nredraw = False
               ,fp = fname
               ,texture = Nothing}

redraw VWR {texture = Nothing} = return ()

redraw gst@VWR {texture = Just txr} = do
   (tx, ty) <- winSize
   let rect = Rect {rectOrig = Point 0 0
                   ,rectWdt = tx
                   ,rectHgt = ty}
   drawTexture txr rect

evt gst'' WindowRefresh = do
  case texture gst'' of
    Nothing -> do
      txi <- loadTexture 0 (fp gst'')
      case txi of
        Left err -> do
          putStrLn err
          exitWith (ExitFailure (-1))
        Right tex -> return gst'' {texture = Just tex}
    _ -> return gst''

evt gst'' _ = return gst''


