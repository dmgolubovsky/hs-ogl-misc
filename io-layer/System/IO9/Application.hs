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
 ,appDefaults
 ,appYaml
 ,AppDescr (..)
 ,AppMode (..)
 ,AppNsAdjust (..)
) where

import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import Control.Exception
import Data.Enumerator
import Data.Nesteratee
import Text.Yaml.EnumTok
import Text.Yaml.Loader
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

-- | Default application settings (at least builtin name should be supplied).

appDefaults :: String -> AppDescr

appDefaults bi = AppDescr {
    appBuiltIn = bi
   ,appMode = AppWait
   ,appNsAdjust = NsClone
   ,appStdIn = Nothing
   ,appStdOut = Nothing
   ,appArgs = []
   ,appPriv = Nothing
  }

-- | Build an application descriptor given the parsed Yaml file (result of the Yaml loader).
-- A suitable Yaml file should contain a mapping at the top level. The following map keys
-- would map to the fields of an 'AppDescr':
--
--  - builtin: string  -> appBuiltIn (required)
--  - mode: call | wait | nowait -> appMode
--  - namespace: share | clone | <list> -> appNsAdjust
--  - stdin: string -> appStdIn
--  - stdout: string -> appStdOut
--  - args: <list> -> appArgs
--  - priv: admin | hostowner | none -> appPriv (world privileges can only be gotten via auth)
-- 
-- Omitted keys result in a default field value (see 'appDefaults'). Incorrect values
-- result in build errors. Unknown keys are ignored. Repeated keys override their
-- previous setting.
-- If a parsed Yaml contains more than one document, only the first document will be used.

yempty = "empty yaml application definition"

appYaml :: [YamlElem] -> AppDescr

appYaml [] = BuildError yempty
appYaml (EError e:_) = BuildError e
appYaml (EDocument _ ns:_) = appn ns

appn [] = BuildError yempty
appn (MkNode {n_elem = EMap mps}:_) =
  let appd = foldl appm (appDefaults "") mps
  in  case appd of
        BuildError e -> BuildError e
        AppDescr {appBuiltIn = ""} -> BuildError "builtin name not set"
        _  -> appd
appn _ = BuildError "expected a mapping at toplevel"

appm (BuildError e) _ = BuildError e
appm app (nkey, nval) = case scalval nkey of
  Just "builtin" -> setbi nval app
  _ -> app

setbi nval app = case scalval nval of
  Just s -> app {appBuiltIn = s}
  Nothing -> BuildError "builtin: string expected"

scalval (MkNode {n_elem = EStr s}) = Just s
scalval _ = Nothing

