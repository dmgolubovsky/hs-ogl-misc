{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}

-- Runner for the ns-base applications.

module Main where

import NsBase
import Data.Typeable
import Data.Data
import Data.Nesteratee
import System.FilePath
import System.Environment.UTF8
import System.IO9.Error
import System.IO9.HostAccess
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad
import Text.Yaml.EnumTok
import Text.Yaml.Loader
import Data.Either
import Data.Either.Unwrap
import System.Console.CmdArgs
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import Control.Exception
--import Data.Enumerator hiding (head, map)
import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B

rootenv = "NS_ROOT"
hostdev = "#Z"
inityaml = "/init.yaml"

data InitArgs = InitArgs {
  r :: FilePath
 ,i :: FilePath
 ,d :: FilePath
 ,pgm :: String
 ,pargs :: [String]
} deriving (Data, Typeable, Show)

main = do
  root <- getEnv rootenv `Control.Exception.catch` (\(e :: IOException) -> return "")
  let omit s = " (\'" ++ s ++ "\' if omitted)"
  let iarg = InitArgs {
        r = root &= help ("path to the root of the host filesystem" ++ omit root) &= typDir
       ,i = inityaml &= help ("path to the initialization configuration file" ++ omit inityaml) 
                     &= typFile
       ,d = hostdev &= help ("root device kernel path" ++ omit hostdev) &= typDir
       ,pgm = def &= argPos 0 &= typFile
       ,pargs = def &= args &= typ "STRING"
      } &= program "ns-base" &= versionArg [ignore]
  iargs <- cmdArgs iarg
  dev <- devHost [(r iargs, "/")]
  nsInit NsBase.apps [dev] $ do
    nsBind BindRepl (d iargs) "/"
    ph <- nsEval (i iargs)
    etks <- readYaml ph
    case etks of
      Left s -> dbgPrint (show s) >> return ()
      Right tks -> do
      let app = appYaml $ loadYaml tks
      pgmres <- nsFork app $ (do
        appBind app
        aph <- nsEval (pgm iargs)
        pgmtks <- readYaml aph
        when (isLeft pgmtks) $ throw $ fromLeft pgmtks
        let pgmapp = appYaml $ loadYaml $ fromRight pgmtks
        sargs <- mapM mapArgument $ map RawArg (pargs iargs)
        z <- nsFork pgmapp $ do
          appBind pgmapp
          nsBuiltIn pgmapp sargs
        nsWait True z) `nsCatch` return
      w <- nsWait True pgmres
      dbgPrint $ show w


