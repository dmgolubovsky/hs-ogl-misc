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

rootdir = "/home/dima/ns-root"

main = do
  args <- getArgs
  let dir = head (args ++ ["/"])
  startns $ do
    lift $ device 'Z' $ devPosix True rootdir
    bindPath BindRepl "#Z" "/"
    bindPath (BindBefore True) "/m2" "/m1"
    e <- evalPath dir
    liftIO . putStrLn . show $ e
    s <- lift $ statfid (epDev e, epFID e)
    liftIO . putStrLn . show $ s
    lift $ devmsg (epDev e) $ Topen (epFID e) c_OREAD
    r <- lift $ devmsg (epDev e) $ Tread (epFID e) 0 1000
    liftIO $ putStrLn $ show r
    
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


