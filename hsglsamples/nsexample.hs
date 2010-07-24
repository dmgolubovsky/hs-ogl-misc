-- Example of namespace usage.

module Main where

import System.IO
import System.Directory
import System.IO9.NameSpace.Pure
import System.IO9.NameSpace.IO
import System.IO9.Devices.DevPosix

import qualified Data.Map as M

rootdir = "/tmp/ns-root"

main = do
  createDirectoryIfMissing True rootdir
  root <- devPosix rootdir
  ns <- return newNameSpace >>= 
        addDevEntry 'Z' root >>=
        mountAt "/" "#Z" BindRepl
  mapM (putStrLn . show) (M.toList ns)

  
