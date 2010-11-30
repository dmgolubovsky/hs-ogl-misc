{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Runner for the ns-base applications.

module Main where

import NsBase
import Data.Typeable
import Data.Data
import Data.Nesteratee
import System.FilePath
import System.Environment.UTF8
import System.IO9.HostAccess
import System.IO9.NameSpaceT
import System.IO9.Application
import Text.Yaml.EnumTok
import Text.Yaml.Loader
import System.Console.CmdArgs
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import Data.Enumerator hiding (head)
import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B

rootdir = "/home/dima/ns-root"

data EchoArgs = EchoArgs {
  n :: Bool
 ,e :: Bool
 ,s :: [String]
} deriving (Data, Typeable, Show)

main = do
  argsx <- getArgs
  dev <- devHost [(rootdir, "/")]
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

