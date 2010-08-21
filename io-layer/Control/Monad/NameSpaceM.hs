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
 ,EvalPath (..)
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
      modify (M.insert (NsPath old) (UnionPoint ud new))
    False -> bind_common fl new old

bindPath fl new old = bind_common fl new old

bind_common fl new old = fail "unimplemented"

-- | Data type to represent an evaluated path. It is returned from the 'evalPath' function.

data EvalPath = EvalPath {
  epDev :: Device                             -- ^ Reference to the device where the path resides
 ,epFID :: FID                                -- ^ FID for the above device and path
 ,epCanon :: FilePath                         -- ^ Canonicalized (with dot-paths) removed path
 ,epEval :: FilePath                          -- ^ Evaluated (starting at the device) path
} deriving (Show)

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, FID of the last path component is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

evalPath :: FilePath -> NameSpaceM EvalPath

evalPath fp | not (isAbsolute fp || isDevice fp) =
  fail "eval: path should be absolute"

evalPath fp = do
  (d, c, e) <- eval_root (splitPath fp) []
  return $ EvalPath (fst d) (snd d) (joinPath c) (joinPath e)

-- Root element. Look it up in the namespace. If not found, fail. If found, obtain its FID,
-- place it on the evaluated path, remove from the unevaluated part, recurse. History must be empty
-- at this point.

eval_root :: [FilePath] -> [FilePath] -> NameSpaceM (DEVFID, [FilePath], [FilePath])

eval_root ("/" : rawps) _ = do
  rootd <- get >>= findroot
  eval_root (rootd : rawps) ["/"]

-- If the path to evaluate is indeed a device path, attach the given device and tree
-- and proceed as if it was the root element.

eval_root (dvp : rawps) xps | isDevice dvp = do
  (dv, fid) <- attdev 2048 dvp
  let orig = head (xps ++ [dvp])
  eval_step (dv, fid) rawps [orig] [dvp]

eval_root _ _ = fail "eval: empty or not absolute/device filepath"

-- Evaluate the rest of the path step-wise.

eval_step :: DEVFID -> [FilePath] -> [FilePath] -> [FilePath] 
  -> NameSpaceM (DEVFID, [FilePath], [FilePath])

-- Entire raw path consumed: evaluation complete.

eval_step dfid [] orig eval = return (dfid, orig, eval)

-- Dot on the path: skip it.

eval_step dfid ("./" : rawps) orig eval = eval_step dfid rawps orig eval
eval_step dfid ("." : rawps) orig eval = eval_step dfid rawps orig eval

-- DotDot: use the logic from http://doc.cat-v.org/plan_9/4th_edition/papers/lexnames
-- If original path consists of only one element, ignore dotdot.
-- If evaluated path consists of only one element, fail: this is an internal error.
-- Take one element off the original path. Lookup the namespace if the resulting original
-- path marks any of the union points. If it does, retrieve its evaluated counterpart:
-- parent may reside on a different filesystem than child. Substitute the retrieved evaluated
-- path to the evaluated path and do the next step.
-- Otherwise just the original path minus one element and evaluated path minus one element.
-- In either case, new FID has to be obtained fo the new evaluated path.

eval_step dfid ("../" : rawps) orig eval = eval_step dfid (".." : rawps) orig eval

eval_step dfid (".." : rawps) [orig] eval = eval_step dfid rawps [orig] eval

eval_step dfid (".." : rawps) orig [eval] = do
  lift . devmsg (fst dfid) $ Tclunk (snd dfid)
  fail $ "eval: internal error: .. with singleton evaluated path at " ++ joinPath orig

eval_step dfid (".." : rawps) orig eval = do
  let chop = reverse . tail . reverse                     -- chop the last element off
      origp = chop orig
      evalp = chop eval
  up <- get >>= return . M.lookup (NsPath $ joinPath origp)
  let neval = case up of
        Just (UnionPoint _ fp) -> splitPath fp
        _ -> evalp
  lift . devmsg (fst dfid) $ Tclunk (snd dfid)
  (ndev, nfid) <- attdev 2048 (head neval)
  wfid <- walkdev (ndev, nfid) (tail neval)
  lift . devmsg ndev $ Tclunk nfid
  eval_step (ndev, wfid) rawps origp neval

-- General case. Look up the already evaluated path in the namespace (resulting in
-- either single or multiple path). One of them may be the one we already have a FID for
-- (that is, same as eval). Try to walk all of these paths to the next element of the
-- path to be evaluated until successful (if not, this is an error). Once the right path 
-- has been found, replace eval with it, and repeat until the path to evaluate is entirely
-- consumed.

eval_step (dev, fid) (rawp : rawps) orig eval = do
  let jeval = joinPath eval
      jorig = joinPath orig
      nrawp = normalise (rawp ++ "/")
  undirs <- get >>= findunion jorig >>= \ps -> return (if null ps then [jeval] else ps)
  ress@(rfp, rdev, rfid) <- foldr mplus (fail $ "eval: cannot walk " ++ (jeval </> nrawp)) $ 
    flip map undirs $ \fp -> do
      (zfp, zdev, zfid, zeval) <- case (fp == jeval) of
        True -> return ([nrawp], dev, fid, eval ++ [nrawp])
        _ -> do
          (xdev, xfid) <- attdev 2048 fp
          let tfp = tail (splitPath fp) ++ [nrawp]
          return (tfp, xdev, xfid, splitPath fp ++ [nrawp])
      wfid <- walkdev (zdev, zfid) zfp
      lift . devmsg zdev $ Tclunk zfid
      return (zeval, zdev, wfid)
  eval_step (rdev, rfid) rawps (orig ++ [rawp]) rfp    

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
-- does not contain the union point provided, return an empty list.
-- No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> NameSpaceM [FilePath]

findunion fp ns = do
  let up = M.lookup (NsPath fp) ns
  case up of
    Just (UnionPoint ud fp) -> return . map dirfp . DL.toList $ unDir ud 
    _ -> return []

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

-- Walk a device from the given DEVFID to the given split path. The function
-- fails if the driver returns an error message as well as if the walk is incomplete.
-- FID for the destination path is returned. The destination path is considered relative
-- to the one source DEVFID is for. QIDs returned by the driver are lost, only their number
-- is counted.

walkdev :: DEVFID -> [FilePath] -> NameSpaceM FID

walkdev (dev, ffid) fps = newfid >>= \tfid -> lift $ do
  (Rwalk rwlk) <- devmsg dev $ Twalk ffid tfid fps
  case (length rwlk, length fps) of
    (1, 0) -> return tfid
    (x, y) | x == y -> return tfid
    _ -> fail $ "eval: cannot walk to " ++ joinPath fps
 

