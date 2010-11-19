
-- Runner for the ns-base applications.

module Main where

import NsBase
import Data.Nesteratee
import System.FilePath
import System.Environment.UTF8
import System.IO9.HostAccess
import System.IO9.NameSpaceT
import System.IO9.Application
import Text.Yaml.EnumTok
import Data.Enumerator hiding (head)
import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B

rootdir = "/home/dima/ns-root"


main = do
  args <- getArgs
  dev <- devHost [(rootdir, "/")]
  let app = head (args ++ ["/"])
  args <- getArgs
  nsInit NsBase.apps [dev] $ do
    nsBind BindRepl "#Z" "/"
    nsBind (BindAfter False) "#c" "/dev"
    nsBind (BindAfter False) "#α" "/bin"
    ph <- nsEval app
    con <- nsEval "/dev/cons"
    readYaml ph
        >>= dbgPrint . show

procYaml :: Nesteratee Token B.ByteString (NameSpaceT IO) ([Token])

procYaml = nestText . nestYaml []


