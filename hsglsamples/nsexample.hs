-- Example of namespace usage.

module Main where

import System.IO
import System.Directory
import System.IO9.NameSpace.Pure
import System.IO9.NameSpace.IO

import qualified Data.Map as M

rootdir = "/tmp/ns-root"

main = do
  createDirectoryIfMissing True rootdir
  ns <- return newNameSpace >>= 
        addHostPrefix rootdir 'Z' >>=
        bindAt "/" "#Z" BindRepl
  mapM (putStrLn . show) (M.toList ns)

  
