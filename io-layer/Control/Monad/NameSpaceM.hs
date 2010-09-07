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
 ,readUnion
 ,addUnion
 ,unionDir
 ,UnionDir (..)
 ,BoundDir (..)
 ,catchNS
 ,mplusX
 ,liftScope
 ,catchScope
) where

import PrivateDefs
import Data.Char
import Data.Word
import Data.Bits
import qualified Data.DList as DL
import Control.Monad
import Control.Monad.NineM
import Control.Monad.State
import Control.Monad.Trans
import System.FilePath
import System.Directory
import System.IO9.Error
import Control.Exception
import System.IO9.Device hiding (get, put)
import System.IO9.NameSpace.Pure
import qualified Data.Map as M
import qualified Control.Exception as E

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
  throw Efilename

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

-- The common case: create new or update an existing entry in the current namespace map
-- storing the canonicalized and evaluated versions of the "old" path given, along with 
-- evaluated version of the "new" path.

bind_common fl new old = liftScope $ do
  epnew <- evalPath new
  epold <- evalPath old
  ns <- get
  let norm = normalise $ epCanon epold ++ "/"
      uds = unionDir (epEval epold)
      udx = addUnion uds (epEval epnew) fl
      up = UnionPoint udx (epEval epold)
      modx _ (UnionPoint ud fp) = UnionPoint (addUnion ud (epEval epnew) fl) fp
      ns' = M.insertWith modx (NsPath norm) up ns
  put ns'
  return ()

-- | Data type to represent an evaluated path. It is returned from the 'evalPath' function.

data EvalPath = EvalPath {
  epDev :: Device                             -- ^ Reference to the device where the path resides
 ,epFID :: FID                                -- ^ FID for the above device and path
 ,epCanon :: FilePath                         -- ^ Canonicalized (with dot-paths) removed path
 ,epEval :: FilePath                          -- ^ Evaluated (starting at the device) path
} deriving (Show)

instance ScopeR EvalPath where
  retains (EvalPath d f _ _) = [(d, f)]

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, FID of the last path component is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

evalPath :: FilePath -> NameSpaceM EvalPath

evalPath fp | not (isAbsolute fp || isDevice fp) =
  throw Efilename

evalPath fp = liftScope $ do
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

eval_root _ _ = throw Efilename

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
  throw $ OtherError $ "internal error: .. with singleton evaluated path at " ++ joinPath orig

eval_step dfid (".." : rawps) orig eval = do
  let chop = reverse . tail . reverse                     -- chop the last element off
      origp = chop orig
      evalp = chop eval
  up <- get >>= return . M.lookup (NsPath $ joinPath origp)
  let neval = case up of
        Just (UnionPoint _ fp) -> splitPath fp
        _ -> evalp
  (ndev, nfid) <- attdev 2048 (head neval)
  (_, wfid) <- lift $ walkdev (ndev, nfid) (tail neval)
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
  ress@(rfp, rdev, rfid) <- foldr mplusX (throw Enonexist) $ 
    flip map undirs $ \fp -> do
      (zfp, zdev, zfid, zeval) <- case (fp == jeval) of
        True -> return ([nrawp], dev, fid, eval ++ [nrawp])
        _ -> do
          (xdev, xfid) <- attdev 2048 fp
          let tfp = tail (splitPath fp) ++ [nrawp]
          return (tfp, xdev, xfid, splitPath fp ++ [nrawp])
      (_, wfid) <- lift $ walkdev (zdev, zfid) zfp
      return (zeval, zdev, wfid)
  eval_step (rdev, rfid) rawps (orig ++ [rawp]) rfp    

-- | Read contents of a union point as list of 'Stat' structures.

readUnion :: FilePath -> NameSpaceM [Stat]

readUnion fp = liftScope $ do
  ep <- evalPath fp
  let fid = head $ retains ep
  st <- lift $ statfid fid
  let mode = qid_typ (st_qid st)
  case (mode .&. c_QTDIR) of
    0 -> throw Enotdir
    _ -> do
      dirs <- get >>= findunion (epCanon ep) >>= 
                      \ps -> return (if null ps then [epEval ep] else ps)
      sts <- forM dirs $ \dir -> (do
        rfid <- attdev 2048 dir
        dfid <- lift $ walkdev rfid (tail $ splitPath dir)
        qid <- lift $ openfid dfid c_OREAD
        rsts <- case qid_typ qid .&. c_QTDIR of
                  0 -> return []
                  _ -> lift $ readdir dfid
        let dc = deviceOf dir
            setdev st = st { st_typ = fromIntegral $ ord dc
                            ,st_dev = fromIntegral $ devRef $ fst dfid}
        return $ map setdev rsts) `catchScope` (\_ -> return [])
      return $ concat sts

-- | A version of 'catchSome' for use in the 'NameSpaceM' monad.

catchNS :: NameSpaceM a -> (SomeException -> NameSpaceM a) -> NameSpaceM a

x `catchNS`y = do
  s <- get
  (r', s') <- lift (runStateT x s `catchSome` (\e -> runStateT (y e) s))
  put s'
  return r'

-- | A version of 'mplus' which catches any possible exception (for use in the 'NameSpaceM' monad).

mplusX :: NameSpaceM a -> NameSpaceM a -> NameSpaceM a

x `mplusX` y = (x `catchNS` (\_ -> fail "need this for mplus")) `mplus` y

-- | Run a NameSpaced action in a scope. Whatever is returned, retains some or none 
-- DEVFIDs for the parent scope. Simple 'lift' would not work here as proper manipulation
-- of namespace state is required.

liftScope :: (ScopeR r) => NameSpaceM r -> NameSpaceM r

liftScope x = do
  s <- get
  (r', s') <- lift $ do
    enterScope
    (xr, xs) <- runStateT x s `catchSome` (\e -> exitScope [] >> E.throw e)
    exitScope (retains xr)
    return (xr, xs)
  put s'
  return r'  

-- | Run a NameSpaced action in a scope with exception handling. The 'liftScope' function
-- rethrows any exception that could be raised inside a namespaced action; this function
-- tries to run another NameSpaced action that acts as a handler. If the latter action
-- throws an exception too, it will be rethrown as in 'liftScope'.

catchScope :: (ScopeR r) => NameSpaceM r -> (SomeException -> NameSpaceM r) -> NameSpaceM r

x `catchScope` y = do
  s <- get
  (r', s') <- lift $ do
    enterScope
    (xr, xs) <- runStateT x s `catchSome` (\e -> runStateT (y e) s) 
                              `catchSome` (\e1 -> exitScope [] >> E.throw e1)
    exitScope (retains xr)
    return (xr, xs)
  put s'
  return r'

-- Find the root entry in the namespace. The root entry is special that it always has
-- one directory bound. So, only a single entry is returned (in the case of multiple
-- directories unioned under the root entry, head of the list is returned). If no
-- root entry found, fail.

findroot :: NameSpace -> NameSpaceM FilePath

findroot ns = do
  fps <- findunion "/" ns
  case fps of
    (fp:_) | isDevice fp -> return fp
    _ -> throw $ OtherError "eval: no root binding in the namespace"

-- Find all files/directories bound at the given union point. If the namespace provided
-- does not contain the union point provided, return an empty list.
-- No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> NameSpaceM [FilePath]

findunion fp ns = do
  let fp' = if fp == "/" then fp else normalise (fp ++ "/")
      up = M.lookup (NsPath fp') ns
  case up of
    Just (UnionPoint ud _) -> return . map dirfp . DL.toList $ unDir ud 
    _ -> return []

-- Attach a device with given letter and tree, get DEVFID for the device/tree.
-- Username is always blank, and authorization is not requested.

attdev :: Word32 -> FilePath -> NameSpaceM DEVFID

attdev _ fp | not (isDevice fp) = return (noDevice, c_NOFID)

attdev bufsz fp = lift . getdev (deviceOf fp) (treeOf fp) $ \d t -> do
  f <- nextInt >>= return . fromIntegral
  devmsg d $ Tversion bufsz "9P2000"
  devmsg d $ Tattach f c_NOFID "" t
  return f
  



