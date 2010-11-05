------------------------------------------------------------------
-- |
-- Module      :  Data.Enumerator.Compose
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Compositon of Iteratees not directly related to NameSpaceT
------------------------------------------------------------------

module Data.Enumerator.Compose (
  nestIter
) where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class
import Data.Enumerator
import qualified Data.Enumerator as DE (head)

nestIter :: (Eq inner, Monoid inner, Monoid outer, Monad m)
         => (outer -> (inner, outer))
         -> Iteratee inner m b
         -> Iteratee outer m b


nestIter f iter = loop False mempty iter where
  loop eof s i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b _ -> Iteratee $ return $ Yield b $ Chunks [s]
      Continue k | eof -> error "nestIter: divergent iteratee"
      Continue k -> loop2 s k
  loop2 s k = do
    mbchk <- DE.head
    case mbchk of
      Nothing -> loop True s (k EOF)
      Just chk -> do
        let (t, s') = f (s `mappend` chk)
        if t == mempty
          then loop2 s' k
          else loop False s' (k $ Chunks [t])


