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
import Control.Monad.Trans
import Control.Monad.State
import System.IO9.DevLayer
import System.IO9.HostAccess
import System.IO9.NameSpaceT

import System.IO9.DirStream
import qualified Data.ByteString as B

rootdir = "/home/dima/ns-root"

untilM p f = do
  x <- f
  if p x then return x
         else untilM p f

main = do
  args <- getArgs
  let dir = head (args ++ ["/"])
  dev <- devHost [(rootdir, "/")]
  initNS [dev] $ do
    dbgPrint "NameSpace"
    bindPath BindRepl "#Z" "/"
    bindPath (BindBefore True) "/m2" "/m1"
    ns <- showNS
    mapM dbgPrint ns
    return ()


{-
  att <- devAttach dev "/"
  putStrLn $ show att
  wlk <- devWalk att dir
  putStrLn $ show wlk
  putStrLn $ show (devqid wlk)
  devStat wlk >>= putStrLn . show
  h <- devOpen wlk c_OREAD
  case qid_typ (devqid wlk) .&. c_QTDIR of
    0 -> printFile h
    _ -> printDir h
-}

printFile h = do
  hGetContents h >>= putStrLn
  return ()


printDir s = do
  untilM (==True) $ do
    b <- hGetLine s
    mapM putStrLn $ wordsBy (==(chr 0)) b
    hIsEOF s
  return ()
 

{-
  startns $ do
    lift $ device 'Z' $ devPosix True rootdir
    bindPath BindRepl "#Z" "/"
    bindPath (BindBefore True) "/m2" "/m1"
    r <- readUnion dir
    liftIO $ mapM_ (putStrLn . fmt) r

fmt :: Stat -> String

fmt st = fmode (st_mode st) ++ " " ++
         [chr (fromIntegral $ st_typ st)] ++ " " ++
         show (st_dev st) ++ " " ++
         st_name st

fmode :: Word32 -> String

fmode mod = fdir mod ++ "" where
  fdir x | x .&. c_DMDIR /= 0 = "d"
  fdir _ = "-"
-}
{-
    e <- evalPath dir
    liftIO . putStrLn . show $ e
    s <- lift $ statfid (epDev e, epFID e)
    liftIO . putStrLn . show $ s
    lift $ devmsg (epDev e) $ Topen (epFID e) c_OREAD
    r <- lift $ readdir (epDev e, epFID e)
    liftIO $ putStrLn $ show r
-}  
  
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


