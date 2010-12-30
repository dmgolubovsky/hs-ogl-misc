--
-- Simple SVG scene viewer. It displays a scene with clickable areas demarked with
-- dotted green lines. Clicking on an active area causes the "label" attribute of
-- the corresponding SVG path to be printed in standard output.
--

import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Data.Maybe
import Data.Shapes2D
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as C
import Data.List (sort, find, sortBy, groupBy)
import qualified Data.Set as S
import qualified Data.Map as M

import Data.SVGPath
import Graphics.UI.GLFWApp

import Text.XML.Light

main :: IO ()

main = do
  args <- getArgs
  fname <- case args of
    (fn:_) -> return fn
    _    -> putStrLn "usage: svgscn <imagefile.ext>" >> exitFailure
  xml <- B.readFile fname >>= return . parseXML . C.toString
  let svg = xml2scene xml initScene
  case svg of
    Nothing -> putStrLn "Invalid SVG file" >> exitFailure
    Just scn -> glfwMain defaultApp {appTitle = scTitle scn
                                    ,winWidth = scWidth scn
                                    ,winHeight = scHeight scn}
                         scn

drawScene scn | null (scTextures scn) = return scn
drawScene scn = do
  f_glClear (fromIntegral (c_GL_COLOR_BUFFER_BIT .|. c_GL_DEPTH_BUFFER_BIT))
  zipWithM drawTexture (scTextures scn) (map imgrect $ scImages scn)
  let polycmp s1 s2 = shape s1 == shape s2
      edgecmp s1 s2 = compare (edgn s1) (edgn s2)
      polys = map (sortBy edgecmp) (polycmp `groupBy` (concatMap S.elems $ M.elems $ scStore scn))
  mapM drawPoly polys
  return scn

initScene scn ScRedraw = drawScene scn
initScene scn e@(ScEvent _) = do
  txrs' <- mapM (loadTexture 0 . imgpath) (scImages scn)
  let txrs = map (either (const $ GLTexture 0 0 0) id) txrs'
      addshape s m = pts2shape (shppts s) s m
      store = foldr addshape (scStore scn) (scShapes scn)
  loopScene scn {scTextures = txrs
                ,scStore = store
                ,scHandler = loopScene} e

loopScene scn ScRedraw = drawScene scn
loopScene scn (ScEvent e) = do
  x <- case e of
         MouseButton b a | b == fromIntegral c_GLFW_MOUSE_BUTTON_LEFT &&
                           a == fromIntegral c_GLFW_PRESS -> do
           (mx, my) <- glMousePosRatio
           let pmx = round $ mx * fromIntegral (scWidth scn)
               pmy = round $ my * fromIntegral (scHeight scn)
           let shps = sort $ ptInside (Point pmx pmy) (scStore scn)
           case shps of
             [] -> return ()
             (s:_) -> putStrLn (shplabel s) >> hFlush stdout
         _ -> return ()
  
  return scn {scHandler = loopScene}

drawPoly [] = return ()
drawPoly es = do
  f_glColor3f 0.2 0.8 0.2
  mapM drawEdge es
  return ()

drawEdge e = do
  f_glEnable (fromIntegral c_GL_LINE_STIPPLE)
  f_glLineWidth 1
  f_glLineStipple 1 0x5555
  f_glBegin (fromIntegral c_GL_LINES)
  f_glVertex2i (fromIntegral $ x $ start e) (fromIntegral $ y $ start e)
  f_glVertex2i (fromIntegral $ x $ end e) (fromIntegral $ y $ end e)
  f_glEnd


instance GLFWAppData SVGScene where
  reshapeProc = reshape
  redrawProc scn x y = scHandler scn scn ScRedraw >> return ()
  eventHandler scn e = scHandler scn scn (ScEvent e)

reshape scn x y = do
  f_glViewport 0  0 (fromIntegral x) (fromIntegral y)
  f_glMatrixMode (fromIntegral c_GL_PROJECTION)
  f_glLoadIdentity
  f_gluOrtho2D 0.0 (fromIntegral $ scWidth scn)  0.0 (fromIntegral $ scHeight scn)
  return ()



