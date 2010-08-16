-- Example of namespace usage.

module Main where

import System.IO
import System.FilePath
import System.Directory
import System.Environment
import Control.Monad.NineM
import Control.Monad.NameSpaceM
import Control.Monad.Trans
import Control.Monad.State
import System.IO9.Device hiding (get, put)
import System.IO9.Devices.DevPosix

rootdir = "/tmp/ns-root"

main = do
  args <- getArgs
  let dir = head (args ++ ["/"])
  createDirectoryIfMissing True rootdir
  startns $ do
    lift $ device 'Z' $ devPosix True rootdir
    bindPath BindRepl "#Z" "/"
    evalPath "/aa" >>= liftIO . putStrLn . show
    
{-
    device 'Z' $ devPosix True rootdir
    d <- freshdev 'Z'
    devmsg d $ Tversion 2048 "9P2000"
    devmsg d $ Tattach 0 0 "" "/"
    st <- devmsg d $ Tstat 0
    liftIO . putStrLn $ show st
    wk <- devmsg d $ Twalk 0 1 (filter (/= "/") $ splitPath dir) 
    liftIO . putStrLn $ show wk
    liftIO (hGetLine stdin) >>= liftIO . putStrLn
-}
    return ()


