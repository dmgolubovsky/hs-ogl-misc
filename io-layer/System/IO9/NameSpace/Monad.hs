------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.Monad
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- NameSpace Layer and Monad Transformer - Monad definition
------------------------------------------------------------------

module System.IO9.NameSpace.Monad (
  NameSpaceT (..)
 ,nsThrow
 ,nsCatch
 ,nsFinally
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Control.Monad.CatchIO as C
import Control.Exception (ErrorCall, throwIO)
import System.IO.Error
import System.IO9.Error
import System.IO9.NameSpace.Types

-- | A monad transformer over an IO-capable monad to provide namespaced I/O API.

instance (MonadIO m) => Monad (NameSpaceT m) where
    return = NameSpaceT . return
    m >>= k = NameSpaceT $ runNameSpaceT . k =<< runNameSpaceT m
    fail msg = NameSpaceT $ fail msg

-- | Throw a 'NineError'.

nsThrow :: (MonadIO m)
        => NineError
        -> NameSpaceT m a

nsThrow = NameSpaceT . liftIO . throwIO

-- | Catch an error occurred during a namespaced operation, and
-- convert into 'NineError' where possible, otherwise just report
-- the error string to the application.

nsCatch :: (C.MonadCatchIO m) 
        => NameSpaceT m a 
        -> (NineError -> NameSpaceT m a) 
        -> NameSpaceT m a

m `nsCatch` h = NameSpaceT $ do
  env <- ask
  let runhnd = \ee -> runNameSpaceT (h ee) `runReaderT` env
  lift ((runNameSpaceT m `runReaderT` env) `C.catches` 
        [C.Handler (\(e :: NineError) -> runhnd e)
        ,C.Handler (\(e :: ErrorCall) -> runhnd $ OtherError $ show e)
        ,C.Handler (\(e :: IOError) -> case e of
           _ | isAlreadyExistsError e -> runhnd Eexist
           _ | isDoesNotExistError e -> runhnd Enonexist
           _ | isIllegalOperation e -> runhnd Eio
           _ | isPermissionError e -> runhnd Eperm
           _ | isEOFError e -> runhnd Ehungup
           _ -> runhnd $ OtherError $ show e)])


-- | Run a computation, and another computation afterward.

nsFinally :: (C.MonadCatchIO m)
          => NameSpaceT m a                    -- ^ Computation to run first
          -> NameSpaceT m b                    -- ^ Computation to run afterward
          -> NameSpaceT m a

m `nsFinally` h = NameSpaceT $ do
  env <- ask
  let rx x = runNameSpaceT x `runReaderT` env
  lift (rx m `C.finally` rx h)


