------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.IO
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Plan9-style union directories and namespace operations - I/O
------------------------------------------------------------------

module System.IO9.NameSpace.IO (
  mountAt
 ,EvalResult (..)
 ,evalPath
) where

import Data.Char
import Control.Monad
import System.FilePath
import System.Directory
import System.IO9.NameSpace.Pure


-- | Mount a directory or a file at the given location. This is an impure version of
-- 'bindAt' also checking for validity of the path being mounted.

mountAt :: FilePath                       -- ^ Mount point ("old")
        -> FilePath                       -- ^ File or directory to mount/add to union ("new")
        -> BindFlag                       -- ^ Bind flags (before, after, allow creation, etc.)
        -> NameSpace                      -- ^ Current namespace
        -> IO NameSpace                   -- ^ Modified namespace

mountAt fp fp2 flg ns = do
  cfp2 <- canonicalizePath fp2
  ex <- doesDirectoryExist cfp2 -- NB must be evaluation of the file path through namespace !!!
  when (not ex && not (isDevice fp2)) . fail $ "directory " ++ fp2 ++ " does not exist"
  bindAt fp cfp2 flg ns

-- | A datatype to hold the result of file path evaluation.

data EvalResult = EvalOK FilePath FilePath -- ^ Evaluation was successful, holds the final path
                                           -- prefix and the last component
                | EvalProgress [FilePath] [FilePath] -- ^ Evaluation in progress, holds the remainder
                                           -- of the path being evaluated, and the prefix of the
                                           -- partial result
                | EvalError String         -- evaluation error, holds the error message 


-- | Evaluate a filepath through the given namespace. The namespace has to have a root
-- binding. The namespace remains unchanged.

evalPath :: FilePath -> NameSpace -> IO EvalResult

evalPath fp ns | isAbsolute fp || isDevice fp = evalpath ns Nothing $ EvalProgress (splitPath fp) []
evalPath fp _ = return $ EvalError $ "file path to evaluate " ++ fp ++ 
                                     " is neither absolute nor device"

-- Actual evaluator (not exported).

evalpath ns mbd (EvalProgress fpd fpr) | null fpd && null fpr = 
  return $ EvalError "file path to evaluate is empty"

-- evalpath ns mbd (EvalProgress (fpdh:fpdt) fpr) = do
  

evalpath _ _ e = return e


