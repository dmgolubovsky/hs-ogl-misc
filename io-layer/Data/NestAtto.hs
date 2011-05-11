------------------------------------------------------------------
-- |
-- Module      :  Data.NestAtto
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Nesting Attoparsec-based parsers (based on attoparsec-enumerator)
------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module Data.NestAtto ( 
  nestParser
) where

import Data.Maybe
import System.IO9.Error
import Data.Nesteratee
import Data.Attoparsec.Text
import qualified Data.Text as T

-- | A 'Nesteratee' receiving strings of 'T.Text' and sending parse results downstream.

nestParser :: (Monad m, Show r) => Parser r -> Nesteratee (Result r) T.Text m NineError

nestParser p = nestFilter (loop T.empty) where
  gettxt = upStream >>= return . fromMaybe T.empty
  loop t = case T.null t of
    True ->  gettxt >>= doparse
    False -> doparse t
  doparse t = handle (parse p t)
  dofeed pk = do
    ft <- gettxt
    handle (pk ft)
  handle r = let errstr = OtherError $ show r in case r of
      Partial pk -> dofeed pk
      Fail rt ss s -> downStream errstr (Fail T.empty ss s) >> loop rt
      Done rt r -> downStream errstr (Done T.empty r) >> loop rt
    

