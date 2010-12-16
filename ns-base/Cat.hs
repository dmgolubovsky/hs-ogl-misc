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
import Control.Monad.CatchIO
import System.Console.CmdArgs
import Data.List
import Data.Maybe
import Data.NineP.Bits
import Data.Nesteratee
import Data.Text (pack)
import Data.Enumerator (run, ($$))

app :: (MonadIO m, MonadCatchIO m) => AppTable m

app = appEntry [("cat", Monadic cat)]

-- cat ignores any redirection arguments and treats everything else as filenames
-- (hence no options).

data CatArgs = CatArgs {
  s :: [String]
} deriving (Data, Typeable, Show)

cat :: (MonadIO m, MonadCatchIO m) => [Argument] -> NameSpaceT m NineError

cat pargs = do
  (cas, reds) <- appCmdArgs pargs $ CatArgs {
        s = def &= args &= typ "Files"
      } &= program "cat" &= summary "cat: concatenate files" &= 
           versionArg [ignore] &= helpArg [ignore]
  phlist <- case s cas of
    [] -> mapM arg2ph ["-"]
    _ -> mapM arg2ph $ s cas
  appout <- nsStdOut
  nsWithBin appout c_OTRUNC $ \out -> do
    mapM_ (catfile out) phlist
    return EmptyStatus

arg2ph "-" = nsStdIn
arg2ph fp = nsEval fp

catfile o f = do
  r <- run (nsEnumBin 1024 f $$ o)
  case r of
    Left err -> nsThrow $ Located (show f) $ OtherError (show err)
    Right _ -> return ()

