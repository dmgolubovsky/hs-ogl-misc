------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.NameSpaceM
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Plan9-style union directories and namespace operations - monad
------------------------------------------------------------------

module Control.Monad.NameSpaceM (
) where

import Data.Char
import Control.Monad
import Control.Monad.NineM
import System.FilePath
import System.Directory
import System.IO9.NameSpace.Pure

-- The NameSpaceM monad is based on the StateT monad transformer no top
-- of the NineM monad. It operates at thread level and encapsulates namespaced
-- I/O operations. Namespace structures are provided to NineM as user state type.
-- Forked thread inherit parent's namespace, and upon completion or notification
-- the parent may incorporate namespace changes done by child threads.



