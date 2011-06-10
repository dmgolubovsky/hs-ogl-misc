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
 ,module Data.Attoparsec.Text
 ,module Data.Attoparsec.Text.FastSet
) where

import Data.Maybe
import System.IO9.Error
import Data.Nesteratee
import Data.Attoparsec.Text
import Data.Attoparsec.Text.FastSet
import qualified Data.Text as T

-- | A 'Nesteratee' receiving strings of 'T.Text' and sending parse results downstream.

nestParser :: (Monad m, Show r) => Parser r -> Nesteratee (Result r) T.Text m NineError

nestParser p = nestFilter (loop (parse p) T.empty) where
  loop pp t = do
    mbtt <- upStream
    case mbtt of
      Nothing | T.null t -> endStream Enoerror
      Nothing -> handle (pp t)
      Just tt -> handle (pp $ T.concat [t, tt])
  nfwl = \_ -> Fail T.empty [] "do not feed wildlife"
  handle r = let errstr = OtherError $ show r in case r of
      Partial pk -> downStream errstr (Partial nfwl) >> loop pk T.empty
      Fail rt ss s -> downStream errstr (Fail T.empty ss s) >> loop (parse p) rt
      Done rt r -> downStream errstr (Done T.empty r) >> loop (parse p) rt
    

