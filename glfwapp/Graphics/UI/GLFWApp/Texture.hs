------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLFWApp.Texture
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  depends on external libraries
-- 
--
--
-- Load and display images as OpenGL textures
------------------------------------------------------------------

-- Code of this module is based in part on the sample OpenGL image
-- viewer and on the bitmap-opengl package by Balazs Komuves:
-- http://code.haskell.org/~bkomuves/projects/stb-image/example/viewer.hs

-- Images are loaded via the stb-image package. Note that the underlying STB code
-- has certain limitations on image files it can load.

module Graphics.UI.GLFWApp.Texture (
  GLTexture (..)
 ,loadTexture
 ,textureSize
 ,drawTexture) where

import HS_AUTOGLFW_H
import Data.Shapes2D
import Data.Bitmap.IO
import Codec.Image.STB
import Control.Monad

-- | A data type for a loaded OpenGL texture.

data GLTexture = GLTexture {
  texName :: !CUInt            -- ^ OpenGL texture number
 ,imgWidth :: !Int             -- ^ Original image width
 ,imgHeight :: !Int            -- ^ Original image height
}

-- | Load an image from a file, convert it to bitmap and generate
-- a texture.

loadTexture :: CUInt           -- ^ Texture number: if 0, new number will be generated;
                               --   otherwise it has to correspond to an existing texture
                               --   which will be replaced.
            -> FilePath        -- ^ Path to the image file.
            -> IO (Either String GLTexture)

loadTexture txn fn = do
  result <- loadImage fn
  case result of
    Left err  -> return $ Left err
    Right img' -> bmp2tex txn img'

-- | Obtain size from a texture. Note that in general it is not the size of
-- the image texture was generated from. Texture size may be used to calculate
-- texture coordinates to bind to rectangle vertices so only the part containing
-- actual image will show.

textureSize :: CUInt -> IO (Int, Int)

textureSize txn = alloca $ \th -> alloca $ \tw -> do
  zipWithM (f_glGetTexLevelParameteriv (fromIntegral c_GL_TEXTURE_2D) 0)
     (map fromIntegral [c_GL_TEXTURE_WIDTH, c_GL_TEXTURE_HEIGHT])
     [tw, th]
  w <- peek tw >>= return . fromIntegral
  h <- peek th >>= return . fromIntegral
  return (w, h)

-- | Draw a texture within a given rectangle. Difference between image aspect and rectangle
-- aspect is not handled here.

drawTexture :: GLTexture -> Rect -> IO ()

drawTexture txr rect = do
   f_glPixelStorei (fromIntegral c_GL_UNPACK_ALIGNMENT) 1
   f_glClear (fromIntegral c_GL_COLOR_BUFFER_BIT)
   f_glEnable (fromIntegral c_GL_TEXTURE_2D)
   f_glTexEnvf (fromIntegral c_GL_TEXTURE_ENV) 
               (fromIntegral c_GL_TEXTURE_ENV_MODE) 
               (fromIntegral c_GL_DECAL)
   let texparm p v = f_glTexParameterf (fromIntegral c_GL_TEXTURE_2D)
                                       (fromIntegral p)
                                       (fromIntegral v)
   zipWithM texparm [c_GL_TEXTURE_MIN_FILTER
                    ,c_GL_TEXTURE_MAG_FILTER
                    ,c_GL_TEXTURE_WRAP_S
                    ,c_GL_TEXTURE_WRAP_T]
                    [c_GL_LINEAR
                    ,c_GL_LINEAR
                    ,c_GL_CLAMP
                    ,c_GL_CLAMP]
   f_glBindTexture (fromIntegral c_GL_TEXTURE_2D) (texName txr)
   let (tx, ty) = (rectWdt rect, rectHgt rect)
       (ox, oy) = (x $ rectOrig rect, y $ rectOrig rect)
       w = imgWidth txr
       h = imgHeight txr
   (tw, th) <- textureSize (texName txr)
   f_glBegin (fromIntegral c_GL_QUADS)
   f_glTexCoord2d 0 (fromIntegral h/fromIntegral th) 
   f_glVertex2i (fromIntegral ox) (fromIntegral oy)
   f_glTexCoord2d (fromIntegral w/fromIntegral tw) (fromIntegral h/fromIntegral th)
   f_glVertex2i (fromIntegral tx) (fromIntegral oy)
   f_glTexCoord2d (fromIntegral w/fromIntegral tw) 0 
   f_glVertex2i (fromIntegral tx) (fromIntegral ty)
   f_glTexCoord2d 0 0
   f_glVertex2i (fromIntegral oy) (fromIntegral ty)
   f_glEnd
   f_glDisable (fromIntegral c_GL_TEXTURE_2D)
   f_glFlush
   return ()

-- Common code to generate texture from bitmap.

bmp2tex txn img' = do
  img <- extendImage img'
  let (x, y) = bitmapSize img'
  txnn <- case txn of
    0 -> alloca $ \t -> do
      f_glGenTextures 1 t
      peek t
    _ -> return txn
  withBitmap img $ \(w, h) nchn padding ptr -> do
    f_glBindTexture (fromIntegral c_GL_TEXTURE_2D) txnn
    let fmt = case nchn of
          1 -> c_GL_LUMINANCE
          2 -> c_GL_LUMINANCE_ALPHA
          3 -> c_GL_RGB
          4 -> c_GL_RGBA
    f_glTexImage2D (fromIntegral c_GL_TEXTURE_2D)
                   0
                   (fromIntegral nchn)
                   (fromIntegral w)
                   (fromIntegral h)
                   0
                   (fromIntegral fmt)
                   (fromIntegral c_GL_UNSIGNED_BYTE)
                   (castPtr ptr)
    return $ Right $ GLTexture {
      texName = txnn
     ,imgWidth = x
     ,imgHeight = y}

-- Utility code from the Balazs Komuves' picture viewer: OpenGL has limitations
-- on bitmap sizes and alignment: pad the generated bitmap to nearest power-of-two
-- sizes.

-- extend the image to have power-of-two sizes, for old videocards

log2 :: Int -> Int
log2 n = case n of
  0 -> -1
  _ -> 1 + log2 (shiftR n 1) 
 
nextPowerOfTwo :: Int -> Int      
nextPowerOfTwo n = 2 ^ ( 1 + log2 (n-1) )
  
extendImage :: Image -> IO Image  
extendImage bm = do
  let (oldx,oldy) = bitmapSize bm
      (newx,newy) = (nextPowerOfTwo oldx, nextPowerOfTwo newx)
  copySubImage' bm (0,0) (oldx,oldy) (newx,newy) (0,0)
 
