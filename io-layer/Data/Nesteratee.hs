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
 ,nestEOF
 ,nestYield
 ,iterFinal
 ,Nested (..)
 ,nestFilter
 ,upStream
 ,downStream
 ,endStream
 ,liftMB
 ,liftMI
) where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Abort
import Control.Monad.Trans.State.Strict
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

nestState :: (Eq i, Eq o, Monoid i, Monoid o, Monad m)
          => (o -> (i, o))                       -- ^ The chunk processing function 
          -> Nesteratee i o m b                  -- ^ The resulting 'Nesteratee'

nestState f iter = loop False mempty iter where
  loop eof s i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b _ -> yield b $ Chunks [s]
      Continue k | eof -> error "nestState: divergent iteratee"
      Continue k -> loop2 s k
  loop2 s k = do
    mbchk <- DE.head
    case mbchk of
      Nothing | s == mempty -> loop True s (k EOF)
      Nothing -> do
        let (t, s') = f s
        if t == mempty
          then loop True s' (k EOF)
          else loop False s' (k $ Chunks [t])
      Just chk -> do
        let (t, s') = f (s `mappend` chk)
        if t == mempty
          then loop2 s' k
          else loop False s' (k $ Chunks [t])

-- | Similar to 'nestState', but the inner 'Iteratee' chunk type does not need to
-- be a 'Monoid'. The function provided is expected to return a non-empty list
-- of chunks to be passed to the inner 'Iteratee' while empty list will not be passed.

nestStateL :: (Eq o, Monoid o, Monad m)
           => (o -> ([i], o))
           -> Nesteratee i o m b

nestStateL f iter = loop False mempty iter where
  loop eof s i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b _ -> yield b $ Chunks [s]
      Continue k | eof -> error "nestStateL: divergent iteratee"
      Continue k -> loop2 s k
  loop2 s k = do
    mbchk <- DE.head
    case mbchk of
      Nothing | s == mempty -> loop True s (k EOF)
      Nothing -> do
        let (t, s') = f s
        if null t
          then loop True s' (k EOF)
          else loop False s' (k $ Chunks t)
      Just chk -> do
        let (t, s') = f (s `mappend` chk)
        if null t
          then loop2 s' k
          else loop False s' (k $ Chunks t)

-- | A 'Nesteratee' which feeds anything it receives from its standard input to the
-- nested 'Iteratee' and also the chunk provided as an argument if 'EOF' is
-- detected on the input stream.

nestEOF :: (Monad m) => [i] -> Nesteratee i i m b

nestEOF t iter = loop False False iter where
  loop eof e i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield b r -> yield b r
      Continue k | eof -> error "nestEOF: divergent iteratee"
      Continue k -> do
        mbchk <- DE.head
        case mbchk of
          Just chk -> loop False False (k $ Chunks [chk])
          Nothing | e -> loop True True (k EOF)
          Nothing -> loop False True (k $ Chunks t)

-- | Feed the inner 'Iteratee' with whatever is received, but once it 'Yield's a value,
-- replace it with the value provided as the first argument.
 
nestYield :: (Monad m) => r -> Iteratee i m b -> Iteratee i m r

nestYield r iter = loop False iter where
  loop eof i = do
    ix <- lift (runIteratee i)
    case ix of
      Error e -> throwError e
      Yield _ x -> yield r x
      Continue k | eof -> error "nestYield: divergent iteratee"
      Continue k -> do
        mbchk <- DE.head
        case mbchk of
          Just chk -> loop False (k $ Chunks [chk])
          Nothing -> loop True (k EOF)

-- | Receive any amount of input stream, yield the last chunk (or 'Nothing') on EOF.
-- This 'Iteratee' extracts the final computation from the chain of 'Nesteratee's.

iterFinal :: (Monad m) => Iteratee a m (Maybe a)

iterFinal = liftFoldL' (\b a -> Just a) Nothing

-- | A type alias for a nested application body function.

type Nested i o m b a = StateT (Iteratee i m b) (AbortT b (Iteratee o m)) a

-- | A 'Nesteratee' which is basically an application program, running in a 
-- state monad transformer where the inner 'Iteratee' is the state. After the body
-- of the nested program finishes, the inner 'Iteratee' is sent 'EOF'. If however
-- the inner 'Iteratee' yields a value prematurely, the nested application program
-- will be aborted. 

nestFilter :: (Monad m)
           => Nested i o m b ()
           -> Nesteratee i o m b
  
nestFilter body iter = do
  ew <- unwrapAbortT (runStateT body iter)
  case ew of
    Left b -> yield b EOF
    Right (x, i) -> loop False i where
      loop eof i = do
        ix <- lift (runIteratee i)
        case ix of
          Error e -> throwError e
          Yield b r -> Iteratee $ return $ Yield (x `seq` b) EOF
          Continue k | eof -> error "nestApp: divergent iteratee"
          Continue k -> loop True (k EOF)

-- | Receive a chunk of 'Stream' from the application's upstream. This function calls
-- 'DE.head'

upStream :: (Monad m) => Nested i o m b (Maybe o)

upStream = liftMI $ DE.head

-- | Send a chunk (only one at a time) to the inner 'Iteratee'. If the inner 'Iteratee'
-- is not ready, abort the application body with the given value (of the same type as 
-- the outer 'Iteratee' would yield when finished).

downStream :: (Monad m) => b -> i -> Nested i o m b () 

downStream abval dstr = do
  iter <- get
  ix <- liftMB $ runIteratee iter
  case ix of
    Error e -> liftMI $ throwError e
    Yield b _ -> lift (abort abval)
    Continue k -> put (k $ Chunks [dstr])

-- | End the stream on behalf of the nested application body. The value provided
-- will be returned as the result of the whole Iteratee stack. The Iteratee downstream
-- will be sent EOF.

endStream :: (Monad m) => b -> Nested i o m b ()

endStream xval = do
  iter <- get
  ix <- liftMB $ runIteratee iter
  case ix of
    Error e -> liftMI $ throwError e
    Yield b _ -> lift (abort xval)
    Continue k -> put (k EOF) >> lift (abort xval)

-- | Perform an action in the base monad of the 'Nesteratee'.

liftMB :: (Monad m) => m a -> Nested i o m b a

liftMB = lift . lift . lift

-- | Perform an action in the outer 'Iteratee' monad.

liftMI :: (Monad m) => Iteratee o m x -> Nested i o m b x

liftMI = lift . lift

