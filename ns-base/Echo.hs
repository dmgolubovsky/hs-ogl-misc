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

{-# Language DeriveDataTypeable #-}

module Echo (app) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad.IO.Class
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import Data.List
import Data.Maybe
import Data.Nesteratee
import Data.Text (pack)

app :: (MonadIO m) => AppTable m

app = appEntry [("echo", echo)]

data EchoArgs = EchoArgs {
  n :: Bool
 ,s :: [String]
} deriving (Data, Typeable, Show)

echo :: (MonadIO m) => Application m

echo pargs = 
  let raw (RawArg s) = [s]
      raw _ = []
      rargs = concatMap raw pargs
      eargs = EchoArgs {
        n = False &= help "do not append the newline character"
       ,s = def &= args &= typ "Arg"
      } &= program "echo" &= summary "echo: print arguments" &= versionArg [ignore]
      mode = process (cmdArgsMode eargs) rargs
      mode' = case mode of
        Left str -> Left str
        Right ca | isJust (cmdArgsHelp ca) -> Left $ fromJust $ cmdArgsHelp ca
        Right ca | isJust (cmdArgsVersion ca) -> Left $ fromJust $ cmdArgsVersion ca
        Right ca -> Right $ cmdArgsValue ca
      hbody = downStream Eio . pack
      ebody = downStream Eio . pack . intercalate " "
      trail False = ["\n"]
      trail True = []
      eapp b = nestText . nestApp b . nestBin
  in  case mode' of
        Right ech -> eapp (ebody $ s ech ++ trail (n ech))
        Left msg -> eapp (hbody msg)

