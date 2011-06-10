------------------------------------------------------------------
-- |
-- Module      :  Sh
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Simple command shell.
------------------------------------------------------------------

module Sh (app) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad
import Control.Monad.IO.Class
import System.Console.CmdArgs
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Nesteratee
import Data.NestAtto
import Data.Text (Text, pack)

app :: (MonadIO m) => AppTable m

app = appEntry [("sh", TextFilter sh)]

data ShArgs = ShArgs {
  s :: [String]
} deriving (Data, Typeable, Show)

sh as = nestParser shparse . nestFilter prtres where
  prtres = do
    mbr <- upStream
    case mbr of
      Nothing -> endStream Enoerror >> return ()
      Just r -> do
        downStream Eio $ pack $ (++ "\n") $ show r
        prtres

stringT s = string (pack s) <?> s

shparse = stringT "foo" <|> stringT "for" <|> stringT "abcde" <|> stringT "reallylong"

