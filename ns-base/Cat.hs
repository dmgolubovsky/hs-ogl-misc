------------------------------------------------------------------
-- |
-- Module      :  Cat
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- cat: concatenate files to standard output.
------------------------------------------------------------------

module Cat (app) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad.IO.Class
import System.Console.CmdArgs
import Data.List
import Data.Maybe
import Data.Nesteratee
import Data.Text (pack)

app :: (MonadIO m) => AppTable m

app = appEntry [("cat", Monadic cat)]

-- cat ignores any redirection arguments and treats everything else as filenames
-- (hence no options).

cat :: (MonadIO m) => Application m

cat = appBodyB $ \pargs -> do
  margs <- liftMB $ mapM mapArgument pargs
  let unraw (RawArg s) = [s]
      unraw _ = []
      rargs = concatMap unraw margs
  case rargs of
    [] -> loop where
            loop = do 
              mbc <- upStream
              case mbc of
                Nothing -> return ()
                Just c -> downStream Eio c >> loop



