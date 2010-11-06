------------------------------------------------------------------
-- |
-- Module      :  System.IO9.Application
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- General support for embedded applications.
------------------------------------------------------------------

module System.IO9.Application (
  Application (..)
 ,Argument (..)
) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import Data.Nesteratee
import qualified Data.Text as T
import qualified Data.ByteString as B


