------------------------------------------------------------------
-- |
-- Module      :  Data.Nesteratee
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Nesting Compositon of Iteratees
------------------------------------------------------------------

module Data.Nesteratee (
  Nesteratee (..)
 ,nestState
 ,nestStateL
) where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class
import Data.Enumerator
import qualified Data.Enumerator as DE (head)

-- | A general type of an 'Iteratee' nesting another 'Iteratee', or 'Nesteratee'.
-- This is basically a special case of 'Enumeratee', but composable in a different way.
-- Most of 'Iteratee's defined in this module belong to this pattern. A 'Nesteratee'
-- is usually augmented with some function that does some transformation over the chunks
-- which are received by the nesting (outer) 'Iteratee' and fed to the nested (inner)
-- 'Iteratee'. With the 'Nesteratee' pattern the focus is made on the return type
-- of the deepest 'Iteratee', and its unprocessed input is simply discarded in the end.

type Nesteratee i o m b = Iteratee i m b -> Iteratee o m b

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
-- >  nestState f1 . nestState f2
-- >
-- where @f1@ may convert from 'ByteString' to 'Text', and @f2@ breaks the 'Text'
-- into newline-separated pieces.

nestState :: (Eq i, Monoid i, Monoid o, Monad m)
          => (o -> (i, o))                       -- ^ The chunk processing function 
          -> Nesteratee i o m b                  -- ^ The resulting 'Nesteratee'

nestState f iter = loop False mempty iter where
  loop eof s i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b _ -> Iteratee $ return $ Yield b $ Chunks [s]
      Continue k | eof -> error "nestState: divergent iteratee"
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

-- | Similar to 'nestState', but the inner 'Iteratee' chunk type does not need to
-- be a 'Monoid'. The function provided is expected to return a non-empty list
-- of chunks to be passed to the inner 'Iteratee' while empty list will not be passed.

nestStateL :: (Monoid o, Monad m)
           => (o -> ([i], o))
           -> Nesteratee i o m b

nestStateL f iter = loop False mempty iter where
  loop eof s i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b _ -> Iteratee $ return $ Yield b $ Chunks [s]
      Continue k | eof -> error "nestState: divergent iteratee"
      Continue k -> loop2 s k
  loop2 s k = do
    mbchk <- DE.head
    case mbchk of
      Nothing -> loop True s (k EOF)
      Just chk -> do
        let (t, s') = f (s `mappend` chk)
        if null t
          then loop2 s' k
          else loop False s' (k $ Chunks t)


