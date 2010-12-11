------------------------------------------------------------------
-- |
-- Module      :  Echo
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- echo: print text arguments to the standard output iteratee.
------------------------------------------------------------------

module Echo (app) where

import System.IO9.Error
import System.IO9.NameSpaceT
import Data.Nesteratee

app :: AppTable IO

app = appEntry [("echo", echo)]

echo :: Application IO

echo args = nestText . nestLines . nestBin . nestYield Enoerror

