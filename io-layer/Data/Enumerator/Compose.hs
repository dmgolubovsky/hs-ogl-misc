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

-- | Create a stateful 'Iteratee' which keeps its state between input chunks,
-- processes input chunks by the function provided, and feeds the inner 'Iteratee'.
-- This pattern is mostly intended to convert between 'ByteString's and 'Text'.
-- The function provided as the first argument is expected to process the input
-- chunk, and return whatever could be processed, and the leftover. An obvoius example
-- is UTF-8 decoding from bytes to characters when the input ByteString chunk may end
-- in the middle of a UTF-8 sequence. Few bytes at the chunk's end cannot be decoded
-- into a character until the next chunk arrives. If the decoding result is not 'mempty'
-- (hence 'Monoid' constraint on the inner 'Iteratee' chunk type) it will be fed to the
-- inner 'Iteratee'. The leftover will be stored in the outer 'Iteratee' state, and possibly
-- 'mappend'ed to whatever was previously kept, hence 'Monoid' constraint on the outer
-- 'Iteratee' chunk type. Iteratees following this pattern can be composed by simple
-- '.' composition:
-- >
-- >  nestIter f1 . nestIter f2
-- >
-- where @f1@ may convert from 'ByteString' to 'Text', and @f2@ breaks the 'Text'
-- into newline-separated pieces.

nestIter :: (Eq inner, Monoid inner, Monoid outer, Monad m)
         => (outer -> (inner, outer))            -- ^ The chunk processing function 
         -> Iteratee inner m b                   -- ^ The "inner" 'Iteratee'
         -> Iteratee outer m b                   -- ^ The "outer" 'Iteratee'


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


