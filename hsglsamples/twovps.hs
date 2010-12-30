--
-- Example program: click on shapes and see them highlight.
-- This version of shclicks sets constant coordinates for clipping
-- planes rather than actual window dimensions, therefore window
-- contents resize as the window resizes.
-- Experiment with multiple viewports.
--

module Main where

import Data.Word
import Data.List
import Data.Maybe
import Data.Shapes2D
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLFWApp
import Control.Monad
import GHC.IO.Buffer
import qualified Data.Map as M
import qualified Data.Set as S

data Vport = Vport {
  ox :: Double
 ,oy :: Double
 ,mx :: Double
 ,my :: Double
 ,vr :: Double
 ,vg :: Double
 ,vb :: Double
} deriving (Show)

data GST = GST {
  edges :: ShapeStore Int
 ,vports :: [Vport]
 ,focus :: Maybe Int
 ,nredraw :: Bool
 ,defW, defH :: Int
} deriving (Show)

setvp :: Int -> Int -> Vport -> IO ()

setvp x y vp = do
  f_glViewport (round $ fromIntegral x * ox vp) (round $ fromIntegral y * oy vp) 
               (round $ fromIntegral x * mx vp) (round $ fromIntegral y * my vp)
  f_glScissor  (round $ fromIntegral x * ox vp) (round $ fromIntegral y * oy vp) 
               (round $ fromIntegral x * mx vp) (round $ fromIntegral y * my vp)
  f_glClearColor (realToFrac $ vr vp) (realToFrac $ vg vp) (realToFrac $ vb vp) 0.0
  f_glClear (fromIntegral (c_GL_COLOR_BUFFER_BIT .|. c_GL_DEPTH_BUFFER_BIT))
  return ()

gstinit = GST {
  edges = emptyStore
 ,vports = [Vport 0.2 0.2 1.0 1.0 0.3 0.2 0.1, Vport 0.0 0.0 0.33 0.33 0.4 0.5 0.6]
 ,focus = Nothing
 ,nredraw = False
 ,defW = winWidth defaultApp
 ,defH = winHeight defaultApp
}

instance GLFWAppData GST where
  needRedraw = nredraw
  redrawProc = redraw
  reshapeProc = reshape
  eventHandler = evt
  doneRedraw gst = gst {nredraw = False}

evt gst evt = case evt of
  MouseButton b a | b == fromIntegral c_GLFW_MOUSE_BUTTON_LEFT && 
                    a == fromIntegral c_GLFW_PRESS -> do
    (mx, my) <- glMousePosRatio
    (vpx, vpy) <- return (mx * 3, my * 3)
    findShape gst (round $ vpx * fromIntegral (defW gst)) (round $ vpy * fromIntegral (defH gst))
  _ -> return $ gst {nredraw = False}

reshape gst x y = do
  f_glViewport 0  0 (fromIntegral x) (fromIntegral y)
  f_glMatrixMode (fromIntegral c_GL_PROJECTION)
  f_glLoadIdentity
  f_gluOrtho2D 0.0 (fromIntegral $ defW gst)  0.0 (fromIntegral $ defH gst)
  return ()

findShape gst mx my = do
  let shps = sort $ ptInside (Point mx my) (edges gst)
      gst' = case shps of
        [] -> gst {focus = Nothing}
        (s:_) -> gst {focus = Just s}
  return $ gst' {nredraw = True}

redraw gst x y = do
  f_glEnable (fromIntegral c_GL_SCISSOR_TEST)
  let polycmp s1 s2 = shape s1 == shape s2
      edgecmp s1 s2 = compare (edgn s1) (edgn s2)
      polys = map (sortBy edgecmp) (polycmp `groupBy` (concatMap S.elems $ M.elems $ edges gst))
      drawvp x y vp = setvp x y vp >> mapM (drawPoly gst) polys
  mapM (drawvp x y) (vports gst)
  f_glDisable (fromIntegral c_GL_SCISSOR_TEST)
  return ()

drawPoly gst [] = return ()
drawPoly gst es = do
  let pn = shape (head es)
      fcs = isJust (focus gst) && fromJust (focus gst) == pn
  case fcs of
    True -> f_glColor3f 0.5 1.0 0.5
    False -> f_glColor3f 0.5 0.5 1.0
  f_glLineWidth 1
  f_glBegin (fromIntegral c_GL_LINES)
  mapM drawEdge es
  f_glEnd
  return ()

-- mkva and drva are an attempt to draw edges with vertex array: kind of works
-- but segfaults when the window is resizing.

mkva :: (Show a, Eq a, Ord a) => [Edge a] -> IO (Buffer Word32)

mkva es = do
  let sw32 = sizeOf (0 :: Word32)
      bsize = length es * 8
  buf <- newBuffer sw32 bsize WriteBuffer
  let pokec p32 e off = do
        let e32 = [x $ start e, y $ start e, x $ end e, y $ end e]
            base = p32 `plusPtr` (off * 4 * sw32)
        zipWithM (pokeElemOff base) [0 ..] e32
  withBuffer buf $ \ptr -> zipWithM (pokec ptr) es [0 ..]
  return buf {bufL = 0, bufR = bsize}

drva :: Buffer Word32 -> IO ()

drva buf = do
  f_glEnableClientState (fromIntegral c_GL_VERTEX_ARRAY)
  withBuffer buf $ \ptr -> do
    f_glVertexPointer 2 (fromIntegral c_GL_INT) 0 (castPtr ptr)
    f_glDrawArrays (fromIntegral c_GL_LINES) 0 (fromIntegral $ bufferElems buf `div` 2)
  f_glDisableClientState (fromIntegral c_GL_VERTEX_ARRAY)

drawEdge e = do
  f_glVertex2i (fromIntegral $ x $ start e) (fromIntegral $ y $ start e)
  f_glVertex2i (fromIntegral $ x $ end e) (fromIntegral $ y $ end e)

main = do
  let mp = pts2shape [Point 10 20, Point 200 250, Point 350 180, Point 100 45, Point 200 50] 1 $
           pts2shape [Point 116 257, Point 228 180, Point 113 79] 1 $
           pts2shape [Point 133 109, Point 175 143, Point 136 100] 4 $
           pts2shape [Point 200 210, Point 250 340, Point 300 270] 2 emptyStore
  glfwMain defaultApp {appTitle = "Shapes and Clicks"} gstinit {edges = mp}


