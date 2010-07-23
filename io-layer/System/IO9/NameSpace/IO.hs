------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.IO
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Plan9-style union directories and namespace operations - I/O
------------------------------------------------------------------

module System.IO9.NameSpace.IO (
  addHostPrefix
) where

import Data.Char
import Control.Monad
import System.FilePath
import System.Directory
import System.IO9.NameSpace.Pure

-- | Add a host prefix to the given namespace. The directory provided must exist.
-- The character provided names the kernel table entry associated with this
-- prefix. The function fails if any error occurs.

addHostPrefix :: FilePath -> Char -> NameSpace -> IO NameSpace

addHostPrefix fp c ns = do
  cfp <- canonicalizePath fp
  ex <- doesDirectoryExist cfp
  when (not ex) . fail $ "addHostPrefix: directory " ++ fp ++ " does not exist"
  addDevEntry c (HostPath cfp) ns

