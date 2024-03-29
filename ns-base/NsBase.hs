------------------------------------------------------------------
-- |
-- Module      :  NsBase
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Applications table for the ns-base package.
------------------------------------------------------------------

module NsBase (apps) where

import System.IO9.NameSpaceT
import Echo
import Cat
import Ls
import Sh

apps :: AppTable IO

apps = appTable [Echo.app, Cat.app, Ls.app, Sh.app]

