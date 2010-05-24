------------------------------------------------------------------
-- |
-- Module      :  Data.Shapes2D
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
--
--
-- Two-dimensional Polygonal Shapes         
------------------------------------------------------------------

module Data.Shapes2D (
  Point (..)
 ,Rect (..)
 ,Edge (..)
 ,ShapeStore
 ,pts2shape
 ,rect2pts
 ,scale2fit
 ,ctrrect
 ,ptInside) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

-- | A datatype for points on a surface.

data Point = Point {
  x :: Int
 ,y :: Int
} deriving (Eq, Ord, Show)

-- | A datatype for rectangles.

data Rect = Rect {
  rectOrig :: Point           -- ^ lower left corner
 ,rectWdt :: Int              -- ^ width in pixels
 ,rectHgt :: Int              -- ^ height in pixels
} deriving (Show)

-- | A datatype for a shape's edge. The type parameter s corresponds to the shape
-- identifier which can be for example a number, but also anything else 
-- that can be compared.

data (Eq s, Ord s) => Edge s = Edge {
  shape :: s
 ,edgn :: Int
 ,start :: Point
 ,end :: Point
} deriving (Eq, Ord, Show)

-- | Storage for shapes. Each shape is stored as a set of edges. Edges are mapped
-- by their highest vertex (Y coordinate maximal) to simplify edge selection
-- when finding the shape a point belongs to. since more than one edge may have
-- the same highest Y value, additional Sets are used.

type ShapeStore s = M.Map Int (S.Set (Edge s))

-- | Create a shape (or part of a shape) from a list of 'Point's. This function creates
-- a number of edges belonging to a given shape. The edges always compose a closed path
-- (the last point automatically connects the first point). The special case, when
-- the list of points has only two members, creates an edge not connected with anything else.

pts2shape :: (Eq s, Ord s) => [Point] -> s -> ShapeStore s -> ShapeStore s
pts2shape [p1, p2] sh s = addedge p1 p2 1 sh s
pts2shape (p1:p2:pts) sh s = p2s p1 (p2:pts) 2 (pts2shape [p1, p2] sh s) where
  p2s p1 [pl] j s = addedge pl p1 j sh s
  p2s p1 (px:py:ps) j s = p2s p1 (py:ps) (j + 1) (addedge px py j sh s)

-- | Given a rectangle, produce a list of Points in the order: left lower, right lower,
-- right upper, left upper (so it can be processed by 'pts2shape').

rect2pts :: Rect -> [Point]

rect2pts r@Rect {rectOrig = Point ox oy} =
  let lflw = rectOrig r
      rtlw = Point (ox + rectWdt r) oy
      rtup = Point (x rtlw) (oy + rectHgt r)
      lfup = Point ox (y rtup)
  in  [lflw, rtlw, rtup, lfup]
      

-- | Scale rectangle `ri' to fit into rectangle `ro', possibly leaving some horizontal
-- or vertical unused space in `ro'. Origin of the result will be same as origin of `ri'

scale2fit :: Rect -> Rect -> Rect

scale2fit ro ri =
  let aspect = ((fromIntegral $ rectWdt ri) / (fromIntegral $ rectHgt ri)) :: Double
      h1 = floor $ (fromIntegral $ rectWdt ro) / aspect
      w2 = floor $ (fromIntegral $ rectHgt ro) * aspect
      f1 = h1 <= rectHgt ro
      f2 = w2 <= rectWdt ro
  in  case (f1, f2) of
        (True,  True) -> ro {rectOrig = rectOrig ri}
        (False, False) -> ri
        (True,  False) -> ro {rectOrig = rectOrig ri, rectHgt = h1}
        (False, True) -> ro {rectOrig = rectOrig ri, rectWdt = w2}

-- | Center rectangle `ri' within rectange `ro'. by adjusting origin of the former.
-- If `ri' does not fit, 'Nothing' is returned.

ctrrect :: Rect -> Rect -> Maybe Rect

ctrrect ro ri | rectWdt ri > rectWdt ro || rectHgt ri > rectHgt ro = Nothing

ctrrect ro@Rect {rectOrig = Point xo yo} ri = 
  let yoff = (rectHgt ro - rectHgt ri) `div` 2
      xoff = (rectWdt ro - rectWdt ri) `div` 2
  in  Just ri {rectOrig = Point (xo + xoff) (yo + yoff)}

-- | Find all shapes the given point is inside of. The function uses a well-known method
-- of running a straight line upwards from the point of interest, and counting intersections
-- with each shape's edges. Shapes with odd number of intersections contain the point.
-- Vertices and edges are not considered being inside their shapes, so if a point
-- lies on an edge, or on a vertex, it does not belong to the shape owning the edge or 
-- the vertex.

ptInside :: (Eq s, Ord s) => Point -> ShapeStore s -> [s]
ptInside pt s = 
  let mx = x pt
      my = y pt
      (m1, m2) = M.split my s
  in  map head $                     -- list of shapes containing the point
      filter (odd . length) $        -- keep lists with odd length
      group $                        -- group by each shape: each list length is N of intersections
      map shape $                    -- select owning shapes
      filter (below mx my) $         -- only those edges lying above the point
      filter (projects mx) $         -- only those edges whose projection on X contains point's x
      concatMap S.elems $ M.elems m2 -- m2 contains edges whose highest vertex is above the point

-- Local functions

projects :: (Eq s, Ord s) => Int -> Edge s -> Bool
projects x e = leftmost e < x && rightmost e > x

below :: (Eq s, Ord s) => Int -> Int -> Edge s -> Bool
below mx my e | not (projects mx e) = False
below mx my e =
  let (xb, xe) = if lowest e == y (start e) then (x (start e), x (end e))
                                            else (x (end e), x (start e))
      xef = realToFrac xe
      xbf = realToFrac xb
      ybf = realToFrac $ lowest e
      yef = realToFrac $ highest e
      xf = realToFrac mx
      yf = realToFrac my
      hmax = yef - ybf
      hproj = ybf + hmax * (xf - xbf) / (xef - xbf)
  in  yf < hproj

addedge :: (Eq s, Ord s) => Point -> Point -> Int -> s -> ShapeStore s -> ShapeStore s
addedge ps pe j sh s =
  let e = Edge {
        shape = sh
       ,edgn = j
       ,start = ps
       ,end = pe}
  in  M.insertWith S.union (highest e) (S.singleton e) s


highest :: (Eq s, Ord s) => Edge s -> Int
highest s = max (y $ start s) (y $ end s)

lowest :: (Eq s, Ord s) => Edge s -> Int
lowest s = min (y $ start s) (y $ end s)

rightmost :: (Eq s, Ord s) => Edge s -> Int
rightmost s = max (x $ start s) (x $ end s)

leftmost :: (Eq s, Ord s) => Edge s -> Int
leftmost s = min (x $ start s) (x $ end s)



