------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLFWApp.SVGScene
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  depends on external libraries
-- 
--
--
-- Load SVG scenery
------------------------------------------------------------------

module Graphics.UI.GLFWApp.SVGScene (
  SVGShape (..)
 ,BgImage (..)
 ,SVGScene (..)
 ,ScMode (..)
 ,ScHandler
 ,xml2scene
) where

import Data.Maybe
import Data.Shapes2D
import Data.SVGPath
import Text.XML.Light
import Graphics.UI.GLFWApp.Texture
import Graphics.UI.GLFWApp.Events


-- | A datatype for polygonal shapes demarking click-sensitive areas of scenes.

data SVGShape = SVGShape {
  shpnum :: Int                      -- ^ shape number, used for ordering
 ,shplabel :: String                 -- ^ shape label
 ,shppts :: [Point]                  -- ^ shape points (vertices)
} deriving (Show)

-- Instances necessary for the point location algorithm.

instance Eq SVGShape where
  s1 == s2 = shpnum s1 == shpnum s2

instance Ord SVGShape where
  compare s1 s2 = compare (shpnum s1) (shpnum s2)
-- | A datatype for scene background images.

data BgImage = BgImage {
  imgpath :: FilePath                -- ^ path to the picture file
 ,imgrect :: Rect                    -- ^ image rectangle (dimensions)
} deriving (Show)

-- | Function type for scene event/redraw handler.

type ScHandler = SVGScene -> ScMode -> IO SVGScene

-- | A datatype for scene itself.

data SVGScene = SVGScene {
  scTitle :: String                  -- ^ scene title, from the dc:title element
 ,scWidth :: Int                     -- ^ scene width, pixels
 ,scHeight :: Int                    -- ^ scene height, pixels
 ,scShapes :: [SVGShape]             -- ^ polygonal shapes as parsed from SVG
 ,scImages :: [BgImage]              -- ^ bacground images as parsed from SVG
 ,scStore :: ShapeStore SVGShape     -- ^ storage for edges
 ,scTextures :: [GLTexture]          -- ^ textures as loaded from bg image files
 ,scHandler ::  ScHandler            -- ^ function to handle events and redrawing
}

-- | A datatype for the event handling function to distinguish between user-induced
-- and redraw events.

data ScMode = ScEvent GLFWEvent | ScRedraw deriving (Eq, Show)

-- | Build a scene from parsed XML contents.

xml2scene :: [Content] -> ScHandler -> Maybe SVGScene

xml2scene xml sch =
  let elems = onlyElems xml
      paths = concatMap (filterElementsName ((== "path") . qName)) elems
      images = concatMap (filterElementsName ((== "image") . qName)) elems
      svg = concatMap (filterElementsName ((== "svg") . qName)) elems
      filter_imgpath (QName {qName = "absref", qPrefix = Just "sodipodi"}) = True
      filter_imgpath _ = False
      filter_attr n = (== n) . qName
      height = numattr (filter_attr "height") $ head svg 
      title = fromMaybe "Untitled" $
              listToMaybe $
              map strContent $
              concatMap (filterElementsName ((== "title") . qName)) elems
      pathids = map (numattr ((== "id") . qName)) paths
      pathlbls = map (fromMaybe "no label" . 
                      findAttrBy ((== "label") . qName)) paths
      pathdefs = map (map (\(x, y) -> Point (round x) (height - round y)) .
                      ptsFromString (0, 0) .
                      fromMaybe "" . 
                      (findAttrBy ((== "d") . qName))) paths
      imgpaths = map (fromMaybe "/dev/null" .
                      findAttrBy filter_imgpath) images
      maybeRead = fmap fst . listToMaybe . reads
      numattr flt elt = fromMaybe (-1) (findAttrBy flt elt >>= maybeRead)
      imgrect elt = Rect {
        rectOrig = Point {
          x = numattr (filter_attr "x") elt
         ,y = numattr (filter_attr "y") elt}
       ,rectWdt = numattr (filter_attr "width") elt
       ,rectHgt = numattr (filter_attr "height") elt}
      imgrects = map imgrect images
      scene = SVGScene {
        scTitle = title
       ,scWidth = numattr (filter_attr "width") $ head svg
       ,scHeight = height
       ,scShapes = zipWith3 SVGShape pathids pathlbls pathdefs
       ,scImages = zipWith BgImage imgpaths imgrects 
       ,scStore = emptyStore
       ,scTextures = []
       ,scHandler = sch
      }
  in  case svg of
        [] -> Nothing
        _ -> Just scene

