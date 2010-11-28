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
import System.IO9.DevLayer
import System.IO9.NameSpaceT
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import Control.Exception
import Data.List
import Data.Maybe
import Data.Enumerator hiding (map)
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
yexpstr xx = BuildError $ xx ++ ": string expected"
yexpchc xx ss = BuildError $ xx ++ ": expected " ++ intercalate " | " ss

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
  Just "mode" -> setmode nval app
  Just "namespace" -> setns nval app
  Just "stdin" -> setsi nval app
  Just "stdout" -> setso nval app
  Just "priv" -> setpriv nval app
  Just "args" -> setargs nval app
  _ -> app

setsi nval app = case scalval nval of
  Just s -> app {appStdIn = Just s}
  Nothing -> yexpstr "stdin"

setso nval app = case scalval nval of
  Just s -> app {appStdOut = Just s}
  Nothing -> yexpstr "stdout"

setbi nval app = case scalval nval of
  Just s -> app {appBuiltIn = s}
  Nothing -> yexpstr "builtin"

setpriv nval app = case scalval nval of
  Just "admin" -> app {appPriv = Just Admin}
  Just "hostowner" -> app {appPriv = Just HostOwner}
  Just "none" -> app {appPriv = Just None}
  Just _ -> yexpchc "priv" ["admin", "hostowner", "none"]
  Nothing -> yexpstr "priv"

setmode nval app = case scalval nval of
  Just "call" -> app {appMode = AppCall}
  Just "wait" -> app {appMode = AppWait}
  Just "nowait" -> app {appMode = AppNoWait}
  Just _ -> yexpchc "mode" ["call", "wait", "nowait"]
  Nothing -> yexpstr "mode"

setns nval app = case scalval nval of
  Just "share" -> app {appNsAdjust = NsShare}
  Just "clone" -> app {appNsAdjust = NsClone}
  Just _ -> yexpchc "namespace" ["share", "clone", "<list>"]
  Nothing -> case listval nval of
    Just [] -> BuildError "namespace: non-empty list of strings expected"
    Just bcs -> app {appNsAdjust = NsBuild bcs}
    Nothing -> yexpchc "namespace" ["share", "clone", "<list>"]

setargs nval app = case listval nval of
  Just bcs | (not $ null bcs) -> app {appArgs = bcs}
  _ -> BuildError "args: non-empty list of strings expected"

scalval (MkNode {n_elem = EStr s}) = Just s
scalval _ = Nothing

listval (MkNode {n_elem = ESeq ns}) = Just $ catMaybes $ map scalval ns
listval _ = Nothing

