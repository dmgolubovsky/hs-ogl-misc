{-# Language CPP, DeriveDataTypeable, ScopedTypeVariables #-}

--
-- Experiments with Handles on directories (thread-backed).
--

module Main where

import GHC.IO.CSPHandle
import qualified Data.DList as DL
import System.FilePath
import Control.Concurrent
import GHC.IO.Device
import System.IO
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import GHC.IO.Handle
import System.Directory
import GHC.IO.Exception
import System.IO.Error

main :: IO ()

main = do
  putStrLn "Testing directory handles"
  uddv <- mkIOUnionDir (bindDirAt (unionDir "/lib") "/usr/lib" (BindAfter False))
  h <- mkFileHandle uddv "/lib:/usr/lib" ReadMode Nothing nativeNewlineMode
  hGetContents h >>= putStrLn >> hFlush stdout
  uddv <- mkIOUnionDir (bindDirAt (unionDir "/bin") "/usr/bin" (BindBefore False))
  h <- mkFileHandle uddv "." ReadMode Nothing nativeNewlineMode
  hGetContents h >>= putStrLn >> hFlush stdout

-- =============================== Union Directory ============================= --

instance (Show a) => Show (DL.DList a) where
  show x = show (DL.toList x)

data BoundDir = BoundDir {
  dirfp :: FilePath
 ,dircr :: Bool} deriving (Show)

-- Imitate Plan9/Inferno directory binding: the boolean flag to allow
-- file creation in the directory.

data BindFlag = BindBefore Bool | BindAfter Bool | BindRepl deriving (Show)

data UnionDir = UnionDir (DL.DList BoundDir) deriving (Show, Typeable)

unionDir :: FilePath -> UnionDir
unionDir fp = UnionDir (DL.singleton BoundDir {dirfp = fp, dircr = True})

bindDirAt :: UnionDir -> FilePath -> BindFlag -> UnionDir
bindDirAt (UnionDir dl) fp bf = case bf of
  BindRepl -> unionDir fp
  BindBefore cr -> UnionDir $ DL.cons (BoundDir {dirfp = fp, dircr = cr}) dl
  BindAfter cr -> UnionDir $ DL.snoc dl (BoundDir {dirfp = fp, dircr = cr})

-- Virtual "device" for a directory, accessible via handle

data IOUnionDir = IOUnionDir {
  dirSP :: !(MVar ContSP)} deriving (Typeable)

-- Implementation of IODevice

instance IODevice IOUnionDir where
  devType = dirType
  ready = dirReady
  close = dirClose

instance CSPIO IOUnionDir where
  getsp = takeMVar . dirSP
  setsp = putMVar . dirSP
  bufsize = const 4096

mkIOUnionDir :: UnionDir -> IO (StreamReader IOUnionDir)

mkIOUnionDir ud = initDirSP ud >>= newMVar >>= return . StreamReader . IOUnionDir

-- Stream processor for directory handle

initDirSP (UnionDir ds) = do
  dirs <- mapM dirList (map dirfp $ DL.toList ds)
  return $ ContReady (readdir (concat dirs))

readdir :: [FilePath] -> ContSPFun

-- Both unget stack and directory contents are exhausted: EOF.

readdir [] _ _ _ _ = return ContEOF

-- There is directory contents not read yet.

readdir (fp:fps) _ _ _ avl = do
  let fpnl = U.fromString (fp ++ "\n")
  return $ ContBuff fpnl (readdir fps)

    
dirType _ = return Directory

dirReady _ write _ = return (not write) -- always ready to be read, 
                                        -- never ready to write

dirClose (IOUnionDir sp) = do
  modifyMVar_ sp (const $ return $ ContErr $ mkIOError EOF "Handle was closed" Nothing Nothing)
  return ()

-- Utility

dirList :: FilePath -> IO [FilePath]

dirList fp = let dot "." = True
                 dot ".." = True
                 dot _ = False in
             getDirectoryContents fp >>= 
             return . filter (not . dot) >>=
             return . map (fp </>) >>= 
             mapM canonicalizePath


