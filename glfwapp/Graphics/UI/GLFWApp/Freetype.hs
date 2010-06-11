------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLFWApp.Freetype
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  depends on external libraries
-- 
--
--
-- Generate and display glyph bitmaps using the Freetype2 library
------------------------------------------------------------------

module Graphics.UI.GLFWApp.Freetype (
  GlyphBM (..)
 ,bitMapGlyph
 ,drawGlyphs) where

import HS_AUTOGLFW_H
import HS_AUTOFT2_H
import Data.Shapes2D
import Data.Bitmap.IO
import Data.Maybe
import Data.Char

-- | A datatype to represent a "Bitmap" generated for a single glyph. Besides
-- the "Bitmap" itself, it contains information necessary to properly position
-- the raster coordinates when drawing glyphs.

data GlyphBM = GlyphBM {
  glBitmap :: Bitmap Word8        -- ^ Bitmap itself
 ,glAdvance :: Point              -- ^ Advance: how to move the raster coordinates after drawing
 ,glBmtop :: Int                  -- ^ Subtract this from the baseline Y
 ,glBmleft :: Int                 -- ^ Add this to the baseline X
} deriving (Show)

-- | Generate bitmap and position information for a single glyph in the given face.
-- This function generates a grayscale bitmap for both vectorized (TTF, Type1) and
-- bitmapped (BDF, PCF) fonts: in the latter case the bitmap is not antialiased.
-- The function obtains the bitmap from Freetype, flipping it upside down to conform
-- the OpenGL coordinate system. For bitmapped fonts, Freetype generates one-bit-per-pixel
-- bitmaps: its bits are spread into bytes, so the returned value does not depend on the
-- face font type. If rasterizing of the glyph fails for any reason, "Nothing" is returned.

bitMapGlyph :: Ptr S_FT_FaceRec_ -> Char -> IO (Maybe GlyphBM)

bitMapGlyph face ch = do
  let cc = fromIntegral (ord ch)
  alloca $ \matrix -> alloca $ \pen -> do
    (matrix, V_xx) <-- 0x10000
    (matrix, V_xy) <-- 0
    (matrix, V_yx) <-- 0
    (matrix, V_yy) <-- (-0x10000)
    (pen, V_x) <-- 0
    (pen, V_y) <-- 0
    f_FT_Set_Transform face matrix pen
    e <- f_FT_Load_Char face cc (fromIntegral c_FT_LOAD_RENDER)
    case e of
      0 -> do
        slot <- face --> V_glyph
        advx <- slot --> V_advance >>= (--> V_x) >>= return . (`div` 64)
        advy <- slot --> V_advance >>= (--> V_y) >>= return . (`div` 64)
        bmp <- slot --> V_bitmap
        top <- slot --> V_bitmap_top
        left <- slot --> V_bitmap_left
        ngr <- bmp --> V_num_grays
        pxm <- bmp --> V_pixel_mode
        w <- bmp --> V_pitch >>= return . fromIntegral
        h <- bmp --> V_rows >>= return . fromIntegral
        b <- bmp --> V_buffer
        case True of
          True | ngr == 256 && pxm == fromIntegral e_FT_PIXEL_MODE_GRAY -> do
            hsbmp <- copyBitmapFromPtr (fromIntegral w, fromIntegral h)
                                       1
                                       0
                                       ((castPtr b) :: Ptr Word8)
                                       Nothing
            return $ Just GlyphBM {glBitmap = hsbmp
                                  ,glAdvance = Point (fromIntegral advx) (fromIntegral advy)
                                  ,glBmtop = fromIntegral top
                                  ,glBmleft = fromIntegral left}
          True | ngr == 1 && pxm == fromIntegral e_FT_PIXEL_MODE_MONO -> do
            let bmsz = fromIntegral (w * h * 8)
            allocaBytes bmsz $ \newPtr -> do
              flipRows w h b (newPtr `plusPtr` (w * 8 * (h - 1)))
              hsbmp <- copyBitmapFromPtr (fromIntegral w * 8, fromIntegral h)
                                         1
                                         0
                                         newPtr
                                         Nothing
              return $ Just GlyphBM {glBitmap = hsbmp
                                    ,glAdvance = Point (fromIntegral advx) (fromIntegral advy)
                                    ,glBmtop = (h - fromIntegral top)
                                    ,glBmleft = fromIntegral left}

          _ -> do
            return Nothing
      _ -> do
        return Nothing

-- | Draw a string of glyphs at the given raster position with possible
-- blending adjustment function. The blending adjustment function is given
-- an integer index of a glyph, so each glyph may be drawn with different
-- color effects. By default, glBlendFunction is called with GL_SRC_COLOR
-- as sfactor and GL_ONE as dfactor; also glBlendEquation is GL_FUNC_ADD
-- which yields white text on any background. Use GL_FUNC_REVERSE_SUBTRACT
-- for glBlendEquation to get black text on any background.
-- Also, glPixelTransfer may adjust color of the text drawn.

drawGlyphs :: Maybe (Int -> IO ()) -> Point -> [GlyphBM] -> IO ()

drawGlyphs mbadj pos bs = drawGlyphs' 0 mbadj pos bs

drawGlyphs' _ _ _ [] = return ()

drawGlyphs' idx mbadj p (bmp:bmps) = do
  f_glRasterPos2i (fromIntegral (x p + glBmleft bmp)) (fromIntegral (y p - glBmtop bmp))
  f_glEnable (fromIntegral c_GL_BLEND)
  withBitmap (glBitmap bmp) $ \(w, h) nchn padding buf -> do
    case mbadj of
      Nothing -> do
        f_glBlendFunc (fromIntegral c_GL_SRC_COLOR) (fromIntegral c_GL_ONE)
        f_glBlendEquation (fromIntegral c_GL_FUNC_ADD)
        return ()
      Just adj -> adj idx
    f_glDrawPixels (fromIntegral w)
                   (fromIntegral h)
                   (fromIntegral c_GL_LUMINANCE)
                   (fromIntegral c_GL_UNSIGNED_BYTE)
                   (castPtr buf)
  f_glDisable (fromIntegral c_GL_BLEND)
  drawGlyphs' (idx + 1) mbadj (Point (x p + x (glAdvance bmp)) (y p + y (glAdvance bmp))) bmps



-- Utility functions (not exported)

-- Copy a bit-per-pixel bitmap (coming from biitmapped X11 fonts) to a grayscale
-- (byte-per-pixel) bitmap turning the destination upside down.

flipRows :: Int -> Int -> Ptr CUChar -> Ptr Word8 -> IO ()

flipRows _ 0 _ _ = return ()

flipRows w h src dst = do
  spreadBits w src dst
  flipRows w (h - 1) (src `plusPtr` w) (dst `plusPtr` ((-8) * w))

spreadBits :: Int -> Ptr CUChar -> Ptr Word8 -> IO ()

spreadBits 0 _ _ = return ()

spreadBits n src dst = do
  byt <- peek src
  let spread nb = do
        let bit = 7 - nb
            res = case testBit byt bit of 
              False -> 0
              True -> 255 :: Word8
        pokeByteOff dst nb res
        return ()
  mapM spread [0 .. 7]
  spreadBits (n - 1) (src `plusPtr` 1) (dst `plusPtr` 8)



