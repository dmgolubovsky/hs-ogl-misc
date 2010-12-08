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
import System.Console.CmdArgs
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import Control.Exception
import Data.Enumerator hiding (head)
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
      res <- nsFork app $ do
--        nsBind BindRepl (d iargs) "/"
        appBind app
        (nsEval (pgm iargs) >>= dbgPrint . show >> return Enoerror) `nsCatch` return
      dbgPrint $ show res
      w <- nsWait True res
      dbgPrint $ show w

{-

data EchoArgs = EchoArgs {
  n :: Bool
 ,e :: Bool
 ,s :: [String]
} deriving (Data, Typeable, Show)

main = do
  argsx <- getArgs
  dev <- devHost [(rootdir, "/")]
  when (null argsx) $ fail "Need at least one argument"
  let init = head argsx
  nsInit NsBase.apps [dev] $ do
    nsBind BindRepl "#Z" "/"
    ph <- nsEval init
    (Right tks) <- readYaml ph
    let app = appYaml $ loadYaml tks
    appBind app
    let ech = EchoArgs {
                n = def &= help "do not output the trailing newline"
               ,e = def &= help "enable interpretation of backslash escapes"
               ,s = def &= args &= typ "STRING"
              } &= program "echo"
    let mode = cmdArgsMode ech
    dbgPrint $ show mode
    let ea = process mode argsx
    dbgPrint $ show ea

-}
{-
  let app = head (argsx ++ ["/"])
  nsInit NsBase.apps [dev] $ do
    nsBind BindRepl "#Z" "/"
    nsBind (BindAfter False) "#c" "/dev"
    nsBind (BindAfter False) "#α" "/bin"
    let ech = EchoArgs {
                n = def &= help "do not output the trailing newline"
               ,e = def &= help "enable interpretation of backslash escapes"
               ,s = def &= args &= typ "STRING"
              } &= program "echo"
    let mode = cmdArgsMode ech
    dbgPrint $ show mode
    let ea = process mode argsx
    dbgPrint $ show ea
    ph <- nsEval app
    (Right tks) <- readYaml ph
    dbgPrint $ show tks
    let ly = loadYaml tks
    dbgPrint $ show $ ly
    dbgPrint $ show $ appYaml ly
-}

procYaml :: Nesteratee Token B.ByteString (NameSpaceT IO) ([Token])

procYaml = nestText . nestYaml []

{-
    ph <- nsEval app
    con <- nsEval "/dev/cons"
    (Right tks) <- readYaml ph
    dbgPrint $ show tks
    let ly = loadYaml tks
    dbgPrint $ show $ ly
    dbgPrint $ show $ appYaml ly
-}

