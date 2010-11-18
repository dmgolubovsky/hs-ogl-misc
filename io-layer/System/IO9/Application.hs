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
  nestYaml
) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import Data.Nesteratee
import Text.Yaml.EnumTok
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString as B

-- | Receive lines of 'T.Text' (use 'nestLines'), tokenize them into the stream of
-- Yaml tokens.

nestYaml :: (MonadIO m) => b -> Nesteratee Token T.Text (NameSpaceT m) b

nestYaml b = nestApp $ loop [] where 
  loop y = do
    mbtx <- upStream
    case mbtx of
      Just tx -> loop (tx : y)
      Nothing -> mapM (downStream b) (tokyaml (reverse y)) >> return b

tokyaml txs = 
  let inp = T.unpack $ T.concat txs
  in  loopTok inp

