-- Example of namespace usage.

module Main where

import Data.Char
import Data.Word
import Data.Bits
import System.IO
import Data.NineP
import Data.NineP.Bits
import Data.List.Split
import System.FilePath
import System.Directory
import System.Environment
import System.IO9.DevLayer
import System.IO9.HostAccess
import System.IO9.NameSpaceT

import System.IO9.DirStream
import qualified Data.ByteString as B
import Data.Enumerator hiding (head)
import qualified Data.Text as T

rootdir = "/home/dima/ns-root"

untilM p f = do
  x <- f
  if p x then return x
         else untilM p f

main = do
  args <- getArgs
  let dir = head (args ++ ["/"])
  dev <- devHost [(rootdir, "/")]
  nsInit [dev] $ do
    dbgPrint "NameSpace"
    nsBind BindRepl "#Z" "/"
    nsBind (BindBefore False) "/m2" "/m1"
    nsBind (BindBefore True) "/m3" "/m1"
    dbgPrint "Begin"
    ph <- nsEval dir
    dbgPrint $ show ph
    nph <- nsCreate ph "test" 0o600
    dbgPrint $ show nph
    zph <- nsEval (phCanon nph)
    dbgPrint $ show zph
    let ren = keepAllStat {st_name = "test2x"}
    rph <- nsWstat zph ren
    dbgPrint $ show rph
    nsRemove rph
    eph <- (nsCreate ph "hello" 0o600) `nsCatch` (\e -> dbgPrint (show e) >>
                                                        dbgPrint "Reusing" >> 
                                                        nsEval (dir </> "hello"))
    eit <- nsIterText eph 0
    run (enumList 2 [T.pack "Hello Привет\n"] $$ eit) >>= dbgPrint . show
    return ()



printFile h = do
  hGetContents h >>= putStrLn
  return ()


printDir s = do
  untilM (==True) $ do
    b <- hGetLine s
    mapM putStrLn $ wordsBy (==(chr 0)) b
    hIsEOF s
  return ()
 


