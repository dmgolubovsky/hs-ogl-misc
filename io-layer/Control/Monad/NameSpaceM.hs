------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.NameSpaceM
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Plan9-style union directories and namespace operations - monad
------------------------------------------------------------------

module Control.Monad.NameSpaceM (
  BindFlag (..)
 ,startns
 ,bindPath
 ,evalPath
 ,addUnion
 ,unionDir
 ,UnionDir (..)
 ,BoundDir (..)
) where

import PrivateDefs
import Data.Char
import Data.Word
import qualified Data.DList as DL
import Control.Monad
import Control.Monad.NineM
import Control.Monad.State
import Control.Monad.Trans
import System.FilePath
import System.Directory
import System.IO9.Device hiding (get, put)
import System.IO9.NameSpace.Pure
import qualified Data.Map as M

-- | Run the "main" program in the NameSpace monad. The main program always starts
-- with an empty namespace. Any changes in the namespace will be saved post-completion
-- in the userState field of the NineM state.

startns :: NameSpaceM () -> IO ()

startns x = let nns = newNameSpace in startup nns $ do
  u <- execStateT x nns
  s <- get
  put s {userState = u}
  return ()

-- | Bind a path somewhere in the namespace. Both paths should be absolute or device, and will be 
-- evaluated. One exception however applies when binding to the "/" old path to the empty 
-- namespace, evaluation does not occur provided that the new path is a device path.
-- If any of paths is neither absolute nor device, failure occurs.

bindPath :: BindFlag                   -- ^ Bind options (before, after, create etc.)
         -> FilePath                   -- ^ New path
         -> FilePath                   -- ^ Old path
         -> NameSpaceM ()              -- ^ No return value, namespace updated under the hood

bindPath _ new old | not ((isAbsolute new || isDevice new) && (isAbsolute old || isDevice old)) =
  fail "bind: both path should be absolute or device"

bindPath fl new old | old == "/" && isDevice new = do
  let flt (NsPath _) _ = True
      flt _ _ = False
  e <- get >>= return . M.null . M.filterWithKey flt
  case e of
    True -> do
      let ud = unionDir new
      modify (M.insert (NsPath old) (UnionPoint ud))
    False -> bind_common fl new old

bindPath fl new old = bind_common fl new old

bind_common fl new old = fail "unimplemented"

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, FID of the last path component is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

evalPath :: FilePath -> NameSpaceM (Device, FID, FilePath)

evalPath fp | not (isAbsolute fp || isDevice fp) =
  fail "eval: path should be absolute"

evalPath fp = do
  (d, e) <- eval_root (splitPath fp)
  return (fst d, snd d, joinPath e)

-- Root element. Look it up in the namespace. If not found, fail. If found, obtain its FID,
-- place it on the evaluated path, remove from the unevaluated part, recurse. History must be empty
-- at this point.

eval_root :: [FilePath] -> NameSpaceM (DEVFID, [FilePath])

eval_root ("/" : rawps) = do
  rootd <- get >>= findroot
  eval_root (rootd : rawps)

-- If the path to evaluate is indeed a device path, attach the given device and tree
-- and proceed as if it was the root element.

eval_root (dvp : rawps) | isDevice dvp = do
  liftIO . putStrLn $ dvp
  liftIO . putStrLn . show $ rawps
  (dv, fid) <- attdev 2048 dvp
  eval_step (dv, fid) rawps [dvp]

eval_root _ = fail "eval: empty or not absolute/device filepath"

-- Evaluate the rest of the path step-wise.

eval_step :: DEVFID -> [FilePath] -> [FilePath] -> NameSpaceM (DEVFID, [FilePath])

-- Entire raw path consumed: evaluation complete.

eval_step dfid [] eval = return (dfid, eval)

-- Dot on the path: skip it.

eval_step dfid ("./" : rawps) eval = eval_step dfid rawps eval
eval_step dfid ("." : rawps) eval = eval_step dfid rawps eval

-- DotDot: take one step up the evaluated path.

eval_step dfid ("../" : rawps) eval = eval_step dfid (".." : rawps) eval

eval_step _ (".." : rawps) [] = fail "eval: cannot go up too far"

eval_step dfid (".." : rawps) eval = 
  eval_step (noDevice, c_NOFID) rawps (reverse . tail . reverse $ eval)

-- General case. Look up the already evaluated path in the namespace (resulting in
-- either single or multiple path). One of them may be the one we already have a FID for
-- (that is, same as eval). Try to walk all of these paths to the next element of the
-- path to be evaluated until successful (if not, this is an error). Once the right path 
-- has been found, replace eval with it, and repeat until the path to evaluate is entirely
-- consumed.

eval_step (dev, fid) (rawp : rawps) eval = do
  let jeval = joinPath eval
      nrawp = normalise (rawp ++ "/")
  undirs <- get >>= findunion jeval
  liftIO $ putStrLn $ show rawp
  liftIO $ putStrLn $ show jeval
  ress@(rfp, rdev, rfid) <- foldr mplus (fail $ "eval: cannot walk " ++ (jeval </> nrawp)) $ 
    flip map undirs $ \fp -> do
      (zfp, zdev, zfid, zeval) <- case (fp == jeval) of
        True | fid /= c_NOFID -> return ([nrawp], dev, fid, eval ++ [nrawp])
        _ -> do
          (xdev, xfid) <- attdev 2048 fp
          let tfp = tail (splitPath fp) ++ [nrawp]
          return (tfp, xdev, xfid, splitPath fp ++ [nrawp])
      wfid <- newfid
      (Rwalk rwlk) <- lift . devmsg zdev $ Twalk zfid wfid zfp
      lift . devmsg zdev $ Tclunk zfid
      case (length rwlk, length zfp) of
        (1, 0) -> return (zeval, zdev, wfid)
        (x, y) | x == y -> return (zeval, zdev, wfid)
        _ -> mzero

  liftIO . putStrLn . show $ ress

  eval_step (rdev, rfid) rawps rfp    

-- Find the root entry in the namespace. The root entry is special that it always has
-- one directory bound. So, only a single entry is returned (in the case of multiple
-- directories unioned under the root entry, head of the list is returned). If no
-- root entry found, fail.

findroot :: NameSpace -> NameSpaceM FilePath

findroot ns = do
  fps <- findunion "/" ns
  case fps of
    (fp:_) | isDevice fp -> return fp
    _ -> fail "eval: no root binding in the namespace"

-- Find all files/directories bound at the given union point. If the namespace provided
-- does not contain the union point provided, just pretend that one exists, consisting only
-- of itself. No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> NameSpaceM [FilePath]

findunion fp ns = do
  let up = M.lookup (NsPath fp) ns
  case up of
    Just (UnionPoint ud) -> return . map dirfp . DL.toList $ unDir ud 
    _ -> return [fp]

-- New number for a FID: just lifted from the underlying monad.

newfid :: NameSpaceM FID

newfid = lift nextInt >>= return . fromIntegral

-- Attach a device with given letter and tree, get DEVFID for the device/tree.
-- Username is always blank, and authorization is not requested.

attdev :: Word32 -> FilePath -> NameSpaceM DEVFID

attdev _ fp | not (isDevice fp) = return (noDevice, c_NOFID)

attdev bufsz fp = newfid >>= \fid -> lift $ do
  d <- freshdev (deviceOf fp)
  devmsg d $ Tversion bufsz "9P2000"
  devmsg d $ Tattach fid c_NOFID "" (treeOf fp)
  return (d, fid)
