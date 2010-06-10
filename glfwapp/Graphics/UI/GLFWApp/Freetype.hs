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

module Graphics.UI.GLFWApp.Freetype

where

import HS_AUTOGLFW_H
import HS_AUTOFT2_H

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



