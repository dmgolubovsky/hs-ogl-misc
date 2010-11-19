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
 ,readYaml
) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import Control.Exception
import Data.Enumerator
import Data.Nesteratee
import Text.Yaml.EnumTok
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import qualified Data.Text as T
import qualified Data.ByteString as B

-- | Receive 'T.Text' (contents of the whole stream, until EOF), tokenize into the list of
-- Yaml tokens. 

nestYaml :: (MonadIO m) => b -> Nesteratee Token T.Text (NameSpaceT m) b

nestYaml b = nestApp $ loop [] where 
  loop y = do
    mbtx <- upStream
    case mbtx of
      Just tx -> loop (tx : y)
      Nothing -> mapM (downStream b) (tokyaml (reverse y)) >> return ()

tokyaml txs = 
  let inp = T.unpack $ T.concat txs
  in  loopTok inp

-- | Given a 'PathHandle', read in the Yaml stream, and return a list of 'Token's (or an error).

readYaml :: (Monad m, MonadCatchIO m) 
         => PathHandle -> NameSpaceT m (Either SomeException [Token])

readYaml ph = run (nsEnumBin 1024 ph $$ nestText $ nestYaml [] consume)

