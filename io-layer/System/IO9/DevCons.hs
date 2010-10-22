------------------------------------------------------------------
-- |
-- Module      :  System.IO9.DevCons
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A virtual device to access the console (stdin/stdout)
------------------------------------------------------------------

module System.IO9.DevCons (
  devCons
) where

import System.IO9.DevLayer
import System.IO9.DevGen

devCons :: DevGen

devCons t = do
  mtop <- genTopDir t
  gentbl <- devGen mtop 'c' t
  return gentbl

