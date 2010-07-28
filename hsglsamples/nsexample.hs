-- Example of namespace usage.

module Main where

import System.IO
import System.Directory
import System.Environment
import System.IO9.NameSpace.Pure
import System.IO9.NameSpace.IO
import System.IO9.Device
import System.IO9.Devices.DevPosix

import qualified Data.Map as M

rootdir = "/tmp/ns-root"

main = do
  args <- getArgs
  let dir = head (args ++ ["/"])
  createDirectoryIfMissing True rootdir
  root <- devPosix True rootdir
  ns <- return newNameSpace >>= 
        addDevEntry 'Z' root
  mapM (putStrLn . show) (M.toList ns)
  resp <- root $ Msg TTversion 0 (Tversion 2048 "9P2000")
  putStrLn $ show resp
  resp1 <- re_cont resp $ Msg TTattach 1 (Tattach 0 0 "" dir)
  putStrLn $ show resp1
  resp2 <- re_cont resp1 $ Msg TTstat 2 (Tstat 0)
  putStrLn $ show resp2

  
