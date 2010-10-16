{-# Language PatternGuards #-}
------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.Util
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- NameSpace Layer and Monad Transformer - internal utility functions
------------------------------------------------------------------

module System.IO9.NameSpace.Util where

import Data.Char
import System.FilePath
import System.IO9.Error
import System.IO9.DevLayer
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Concurrent
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import GHC.IO (catchException)
import qualified Data.DList as DL
import qualified Data.Map as M


bind_common :: BindFlag -> FilePath -> FilePath -> M.Map Char DevTable 
            -> NameSpace -> IO NameSpace

bind_common fl new old dtb ns = do
  let evalpath fp = eval_common fp `runReaderT` (dtb, ns)
  epnew <- evalpath new
  epold <- evalpath old
  let norm = normalise $ phCanon epold ++ "/"
      oldpath = show $ phAttach epold
      newpath = show $ phAttach epnew
      uds = unionDir epold
      udx = addUnion uds epnew fl
      up = UnionPoint udx oldpath
      modx _ (UnionPoint ud fp) = UnionPoint (addUnion ud epnew fl) fp
      ns' = M.insertWith modx norm up ns
  return ns'

-- Common function for path evaluation use by both nsBind and nsEval.
-- It operates on the immutable copy of the namespace, thus it uses a separate
-- reader transformer.

type EvalM = ReaderT (DevMap, NameSpace) IO

eval_common :: FilePath -> EvalM PathHandle

eval_common fp = eval_root (splitPath fp) []

-- Root element. Look it up in the namespace. If not found, fail. If found,
-- place it on the evaluated path, remove from the unevaluated part, recurse. 
-- History must be empty at this point.

eval_root :: [FilePath] -> [FilePath] -> EvalM PathHandle

eval_root ("/" : rawps) _ = do
  rootd <- asks snd >>= findroot
  eval_root (rootd : rawps) ["/"]

-- If the path to evaluate is indeed a device path, attach the given device and tree
-- and proceed as if it was the root element.

eval_root (dvp : rawps) xps | isDevice dvp = do
  da <- attdev dvp
  let orig = head (xps ++ [dvp])
  eval_step da rawps [orig] [dvp]

eval_root _ _ = liftIO $ throwIO Efilename

-- Evaluate the rest of the path step-wise.

eval_step :: DevAttach -> [FilePath] -> [FilePath] -> [FilePath] -> EvalM PathHandle

-- Entire raw path consumed: evaluation complete.

eval_step da [] orig eval = return PathHandle {
  phAttach = da
 ,phCanon = joinPath orig}

-- Dot on the path: skip it.

eval_step da ("./" : rawps) orig eval = eval_step da rawps orig eval
eval_step da ("." : rawps) orig eval = eval_step da rawps orig eval

-- DotDot: use the logic from http://doc.cat-v.org/plan_9/4th_edition/papers/lexnames
-- If original path consists of only one element, ignore dotdot.
-- If evaluated path consists of only one element, fail: this is an internal error.
-- Take one element off the original path. Lookup the namespace if the resulting original
-- path marks any of the union points. If it does, retrieve its evaluated counterpart:
-- parent may reside on a different filesystem than child. Substitute the retrieved evaluated
-- path to the evaluated path and do the next step.
-- Otherwise just the original path minus one element and evaluated path minus one element.
-- In either case, new FID has to be obtained fo the new evaluated path.

eval_step da ("../" : rawps) orig eval = eval_step da (".." : rawps) orig eval

eval_step da (".." : rawps) [orig] eval = eval_step da rawps [orig] eval

eval_step da (".." : rawps) orig [eval] = do
  throw $ OtherError $ "internal error: .. with singleton evaluated path at " ++ joinPath orig

eval_step da (".." : rawps) orig eval = do
  let chop = reverse . tail . reverse                     -- chop the last element off
      origp = chop orig
      evalp = chop eval
  up <- asks snd >>= return . M.lookup (joinPath origp)
  let neval = case up of
        Just (UnionPoint _ fp) -> splitPath fp
        _ -> evalp
  nda <- attdev (head neval)
  wda <- liftIO $ devWalk nda (joinPath $ tail neval)
  eval_step wda rawps origp neval

-- General case. Look up the already evaluated path in the namespace (resulting in
-- either single or multiple path). One of them may be the one we already have a DevAttach for
-- (that is, same as eval). Try to walk all of these paths to the next element of the
-- path to be evaluated until successful (if not, this is an error). Once the right path 
-- has been found, replace eval with it, and repeat until the path to evaluate is entirely
-- consumed.

eval_step da (rawp : rawps) orig eval = do
  let jeval = joinPath eval
      jorig = joinPath orig
      nrawp = normalise (rawp ++ if null rawps then "" else "/")
  undirs <- asks snd >>= return . findunion jorig >>=  return . map dirph >>=
    \ps -> return (if null ps then [da] else map phAttach ps)
  foldr mplus (throw Enonexist) $ flip map undirs $ \dda -> do
    wda <- liftIO $ (devWalk dda nrawp `catchException` (\(e :: NineError) -> fail ""))
    eval_step wda rawps (orig ++ [rawp]) (eval ++ [rawp])


 

-- Find the root entry in the namespace. The root entry is special that it always has
-- one directory bound. So, only a single entry is returned (in the case of multiple
-- directories unioned under the root entry, head of the list is returned). If no
-- root entry found, fail.

findroot :: NameSpace -> EvalM FilePath

findroot ns = do
  let fps = map dirph $ findunion "/" ns
  case fps of
    (PathHandle {phCanon = fp}:_) | isDevice fp -> return fp
    _ -> liftIO $ throwIO $ OtherError "eval: no root binding in the namespace"

-- Find all files/directories bound at the given union point. If the namespace provided
-- does not contain the union point provided, return an empty list.
-- No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> [BoundDir]

findunion fp ns = 
  let fp' = if fp == "/" then fp else normalise (fp ++ "/")
      up = M.lookup fp' ns
  in  case up of
        Just (UnionPoint ud _) -> DL.toList $ unDir ud 
        _ -> []

-- Do something with a NameSpace, taking care of exceptions.
-- A current NameSpace is referred to by a MVar, and thus can be shared among several
-- threads. Usual takeMVar/putMVar mechanism provides serialized access to the NameSpace.
-- Here, the logic similar to one used with Handles, is employed: if during an operation
-- over a NameSpace an exception happens, the handler will restore the MVar contents.

withNameSpace :: MVar NameSpace -> (NameSpace -> IO NameSpace) -> IO ()

withNameSpace mv f = do
  ns' <- do_operation mv f
  putMVar mv ns'
  return ()

do_operation mv f = do
  ns <- takeMVar mv
  f ns `catchException` handler ns
  where
    handler ns e = do
      putMVar mv ns
      case () of
        _ | Just async_ex <- fromException e -> do
            let _ = async_ex :: AsyncException
            t <- myThreadId
            throwTo t e
            do_operation mv f
        _otherwise ->
            throwIO e

-- Attach a device using the given path (must be a device path)

attdev :: FilePath -> EvalM DevAttach

attdev fp | not (isDevice fp) = liftIO $ throwIO Ebadsharp

attdev fp = do
  kt <- asks fst
  let mbdt = M.lookup (deviceOf fp) kt
  liftIO $ case mbdt of
    Nothing -> throwIO Ebadsharp
    Just dt -> devAttach dt (treeOf fp)
  

-- | Extract the device letter (if any) from the path. If this is not a device path,
-- the 0 character is returned.

deviceOf :: FilePath -> Char

deviceOf ('#':d:_) = d
deviceOf _ = chr 0

-- | Extract the device file tree (if any) from the path. If this is not a device path
-- an empty string is returned.

treeOf :: FilePath -> FilePath

treeOf fp@('#':_) = let (('#':_:tree):_) = splitPath fp in case tree of
  "" -> "/"
  _ -> tree
treeOf _ = ""


dbgPrint :: MonadIO m => String -> NameSpaceT m ()

dbgPrint s = NameSpaceT $ liftIO $ putStrLn s

-- | Create an union directory containing a single directory. file creation
-- is enabled by default.

unionDir :: PathHandle -> UnionDir
unionDir ph = UnionDir (DL.singleton BoundDir {dirph = ph, dirfl = BindRepl, dircr = True})

-- | Bind an actual directory at the given union point.

addUnion  :: UnionDir              -- ^ The union point where to bind a directory
          -> PathHandle            -- ^ Actual file path to bind (not checked for existence)
          -> BindFlag              -- ^ Bind mode
          -> UnionDir              -- ^ Updated union point.
addUnion (UnionDir dl) ph bf = case bf of
  BindRepl -> unionDir ph
  BindBefore cr -> UnionDir $ DL.cons (BoundDir {dirph = ph, dirfl = bf, dircr = cr}) dl
  BindAfter cr -> UnionDir $ DL.snoc dl (BoundDir {dirph = ph, dirfl = bf, dircr = cr})

