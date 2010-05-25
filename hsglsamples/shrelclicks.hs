--
-- Example program: click on shapes and see them highlight.
-- This version of shclicks sets constant coordinates for clipping
-- planes rather than actual window dimensions, therefore window
-- contents resize as the window resizes.
--

module Main where

import Data.List
import Data.Maybe
import Data.Shapes2D
import Graphics.UI.GLFWApp
import qualified Data.Map as M
import qualified Data.Set as S

data GST = GST {
  edges :: ShapeStore Int
 ,focus :: Maybe Int
 ,nredraw :: Bool
 ,defW, defH :: Int
} deriving (Show)

gstinit = GST {
  edges = M.empty
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

evt gst evt = case evt of
  MouseButton b a | b == fromIntegral c_GLFW_MOUSE_BUTTON_LEFT && 
                    a == fromIntegral c_GLFW_PRESS -> do
    (mx, my) <- glMousePosRatio
    findShape gst (round $ mx * 400) (round $ my * 400)
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

redraw gst = do
  let polycmp s1 s2 = shape s1 == shape s2
      edgecmp s1 s2 = compare (edgn s1) (edgn s2)
      polys = map (sortBy edgecmp) (polycmp `groupBy` (concatMap S.elems $ M.elems $ edges gst))
  f_glClear (fromIntegral (c_GL_COLOR_BUFFER_BIT .|. c_GL_DEPTH_BUFFER_BIT))
  mapM (drawPoly gst) polys
  return ()

drawPoly gst [] = return ()
drawPoly gst es = do
  let pn = shape (head es)
      fcs = isJust (focus gst) && fromJust (focus gst) == pn
  case fcs of
    True -> f_glColor3f 0.5 1.0 0.5
    False -> f_glColor3f 0.5 0.5 1.0
  mapM drawEdge es
  return ()

drawEdge e = do
  f_glLineWidth 1
  f_glBegin (fromIntegral c_GL_LINES)
  f_glVertex2i (fromIntegral $ x $ start e) (fromIntegral $ y $ start e)
  f_glVertex2i (fromIntegral $ x $ end e) (fromIntegral $ y $ end e)
  f_glEnd

main = do
  let mp = pts2shape [Point 10 20, Point 200 250, Point 350 180, Point 100 45, Point 200 50] 1 $
           pts2shape [Point 116 257, Point 228 180, Point 113 79] 1 $
           pts2shape [Point 133 109, Point 175 143, Point 136 100] 4 $
           pts2shape [Point 200 200, Point 250 340, Point 300 270] 2 M.empty
  glfwMain defaultApp {appTitle = "Shapes and Clicks"} gstinit {edges = mp}


