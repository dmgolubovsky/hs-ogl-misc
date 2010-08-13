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
 ,addUnion
 ,unionDir
 ,UnionDir (..)
 ,BoundDir (..)
) where

import PrivateDefs
import Data.Char
import Control.Monad
import Control.Monad.NineM
import Control.Monad.State
import Control.Monad.Trans
import System.FilePath
import System.Directory
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

