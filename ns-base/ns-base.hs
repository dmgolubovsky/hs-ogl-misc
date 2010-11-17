-- Runner for the ns-base applications.

module Main where

import NsBase
import System.FilePath
import System.Environment
import System.IO9.HostAccess
import System.IO9.NameSpaceT
import Data.Enumerator hiding (head)
import qualified Data.DList as D
import qualified Data.Map as M

rootdir = "/home/dima/ns-root"


main = do
  args <- getArgs
  dev <- devHost [(rootdir, "/")]
  let app = head (args ++ ["/"])
  args <- getArgs
  nsInit NsBase.apps [dev] $ do
    nsBind BindRepl "#Z" "/"
    nsBind (BindAfter False) "#α" "/bin"
    ph <- nsEval app
    con <- nsEval "/dev/cons"
    nsWithText con 0 $ \c -> do
      run (nsEnumDir ph $$ dbgChunks True)
        >>= dbgPrint . show

    
