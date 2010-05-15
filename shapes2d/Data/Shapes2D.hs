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
 ,Edge (..)
 ,ShapeStore) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

-- | A datatype for points on a surface.

data Point = Point {
  x :: Int
 ,y :: Int
} deriving (Eq, Ord, Show)

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


