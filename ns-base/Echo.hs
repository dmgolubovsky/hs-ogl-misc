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
import System.IO9.Application
import Control.Monad.IO.Class
import System.Console.CmdArgs
import Data.List
import Data.Maybe
import Data.Nesteratee
import Data.Text (Text, pack)

app :: (MonadIO m) => AppTable m

app = appEntry [("echo", TextFilter echo)]

data EchoArgs = EchoArgs {
  n :: Bool
 ,s :: [String]
} deriving (Data, Typeable, Show)

echo :: (MonadIO m) => Filter m Text

echo = appBodyT $ \pargs -> do
  (ech, reds) <- liftMB $ appCmdArgs pargs $ EchoArgs {
        n = False &= help "do not append the newline character"
       ,s = def &= args &= typ "Arg"
      } &= program "echo" &= summary "echo: print arguments" &= versionArg [ignore]
  let ebody = downStream Eio . pack . intercalate " "
      trail False = ["\n"]
      trail True = []
  ebody $ s ech ++ trail (n ech)

