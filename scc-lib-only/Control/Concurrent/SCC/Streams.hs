{- 
    Copyright 2010 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | This module defines 'Source' and 'Sink' types and 'pipe' functions that create them. The method 'get' on 'Source'
-- abstracts away 'Control.Concurrent.Coroutine.await', and the method 'put' on 'Sink' is a higher-level abstraction of
-- 'Control.Concurrent.Coroutine.SuspensionFunctors.yield'. With this arrangement, a single coroutine can yield values
-- to multiple sinks and await values from multiple sources with no need to change the
-- 'Control.Concurrent.Coroutine.Coroutine' functor; the only requirement is for each funtor of the sources and sinks
-- the coroutine uses to be an 'Control.Concurrent.Coroutine.AncestorFunctor' of the coroutine's functor. For example,
-- coroutine /zip/ that takes two sources and one sink would be declared like this:
-- 
-- @
-- zip :: forall m a1 a2 a3 d x y. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
--        => Source m a1 x -> Source m a2 y -> Sink m a3 (x, y) -> Coroutine d m ()
-- @
-- 
-- Sources, sinks, and coroutines communicating through them are all created using the 'pipe' function or one of its
-- variants. They effectively split the current coroutine into a producer-consumer coroutine pair. The producer gets a
-- new 'Sink' to write to and the consumer a new 'Source' to read from, in addition to all the streams that are visible
-- in the original coroutine. The following function, for example, uses the /zip/ coroutine above to add together the
-- values from two Integer sources:
--
-- @
-- add :: forall m a1 a2 a3 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
--        => Source m a1 Integer -> Source m a2 Integer -> Sink m a3 Integer -> Coroutine d m ()
-- add source1 source2 sink = do pipe
--                                  (\pairSink-> zip source1 source2 pairSink)              -- producer coroutine
--                                  (\pairSource-> mapStream (uncurry (+)) pairSource sink) -- consumer coroutine
--                               return ()
-- @

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeFamilies, KindSignatures #-}

module Control.Concurrent.SCC.Streams
   (
    -- * Sink and Source types
    Sink, Source, SinkFunctor, SourceFunctor, AncestorFunctor,
    -- * Sink and Source constructors
    pipe, pipeP, pipePS, nullSink, nullSource,
    -- * Operations on sinks and sources
    -- ** Singleton operations
    get, put, getWith,
    -- ** Lifting functions
    liftSink, liftSource,
    -- ** Bulk operations
    pour, tee, teeSink, teeSource,
    mapStream, mapSource, mapSink, mapMStream, mapMSource, mapMSink, mapMStream_,
    mapMaybeStream, mapMaybeSink, mapMaybeSource,
    filterMStream, filterMSource, filterMSink,
    foldStream, foldMStream, foldMStream_, mapAccumStream, partitionStream,
    unfoldMStream, unmapMStream_,
    zipWithMStream, parZipWithMStream,
    getList, putList, putQueue,
    -- * Utility functions
    cond
   )
where

import qualified Control.Monad
import qualified Data.List
import qualified Data.Maybe

import Control.Monad (liftM, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, viewl)

import Control.Monad.Parallel (MonadParallel(..))
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Await(Await), Yield(Yield), EitherFunctor(..), await, yield)
import Control.Monad.Coroutine.Nested (AncestorFunctor(..), liftOut, seesawNested)

type SourceFunctor a x = EitherFunctor a (Await (Maybe x))
type SinkFunctor a x = EitherFunctor a (Yield x)

-- | A 'Sink' can be used to yield values from any nested `Coroutine` computation whose functor provably descends from
-- the functor /a/. It's the write-only end of a communication channel created by 'pipe'.
newtype Sink (m :: * -> *) a x =
   Sink
   {
   -- | This function puts a value into the given `Sink`. The intervening 'Coroutine' computations suspend up
   -- to the 'pipe' invocation that has created the argument sink.
   put :: forall d. AncestorFunctor a d => x -> Coroutine d m ()
   }

-- | A 'Source' can be used to read values into any nested `Coroutine` computation whose functor provably descends from
-- the functor /a/. It's the read-only end of a communication channel created by 'pipe'.
newtype Source (m :: * -> *) a x =
   Source
   {
   -- | Function 'get' tries to get a value from the given 'Source' argument. The intervening 'Coroutine' computations
   -- suspend all the way to the 'pipe' function invocation that created the source. The function returns 'Nothing' if
   -- the argument source is empty.
   get :: forall d. AncestorFunctor a d => Coroutine d m (Maybe x)
   }

-- | A disconnected sink that ignores all values 'put' into it.
nullSink :: forall m a x. Monad m => Sink m a x
nullSink = Sink{put= const (return ())}

-- | An empty source whose 'get' always returns Nothing.
nullSource :: forall m a x. Monad m => Source m a x
nullSource = Source{get= return Nothing}

-- | Converts a 'Sink' on the ancestor functor /a/ into a sink on the descendant functor /d/.
liftSink :: forall m a d x. (Monad m, AncestorFunctor a d) => Sink m a x -> Sink m d x
liftSink s = Sink {put= liftOut . (put s :: x -> Coroutine d m ())}

-- | Converts a 'Source' on the ancestor functor /a/ into a source on the descendant functor /d/.
liftSource :: forall m a d x. (Monad m, AncestorFunctor a d) => Source m a x -> Source m d x
liftSource s = Source {get= liftOut (get s :: Coroutine d m (Maybe x))}

-- | The 'pipe' function splits the computation into two concurrent parts, /producer/ and /consumer/. The /producer/ is
-- given a 'Sink' to put values into, and /consumer/ a 'Source' to get those values from. Once producer and consumer
-- both complete, 'pipe' returns their paired results.
pipe :: forall m a a1 a2 x r1 r2. (Monad m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
        (Sink m a1 x -> Coroutine a1 m r1) -> (Source m a2 x -> Coroutine a2 m r2) -> Coroutine a m (r1, r2)
pipe = pipeG (\ f mx my -> do {x <- mx; y <- my; f x y})

-- | The 'pipeP' function is equivalent to 'pipe', except it runs the /producer/ and the /consumer/ in parallel.
pipeP :: forall m a a1 a2 x r1 r2. (MonadParallel m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
         (Sink m a1 x -> Coroutine a1 m r1) -> (Source m a2 x -> Coroutine a2 m r2) -> Coroutine a m (r1, r2)
pipeP = pipeG bindM2

-- | The 'pipePS' function acts either as 'pipeP' or as 'pipe', depending on the argument /parallel/.
pipePS :: forall m a a1 a2 x r1 r2. (MonadParallel m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
          Bool -> (Sink m a1 x -> Coroutine a1 m r1) -> (Source m a2 x -> Coroutine a2 m r2) ->
          Coroutine a m (r1, r2)
pipePS parallel = if parallel then pipeP else pipe

-- | A generic version of 'pipe'. The first argument is used to combine two computation steps.
pipeG :: forall m a a1 a2 x r1 r2. (Monad m, Functor a, a1 ~ SinkFunctor a x, a2 ~ SourceFunctor a x) =>
         (forall x y r. (x -> y -> m r) -> m x -> m y -> m r)
      -> (Sink m a1 x -> Coroutine a1 m r1) -> (Source m a2 x -> Coroutine a2 m r2)
      -> Coroutine a m (r1, r2)
pipeG run2 producer consumer =
   liftM (uncurry (flip (,))) $ seesawNested run2 resolver (consumer source) (producer sink)
   where sink = Sink {put= liftOut . (mapSuspension RightF . yield :: x -> Coroutine a1 m ())} :: Sink m a1 x
         source = Source (liftOut (mapSuspension RightF await :: Coroutine a2 m (Maybe x))) :: Source m a2 x
         resolver = SeesawResolver {
                      resumeLeft = \(Await c)-> c Nothing,
                      resumeRight= \(Yield _ c)-> c,
                      resumeAny= \ _ resumeProducer resumeBoth (Await cc) (Yield x cp) -> resumeBoth (cc (Just x)) cp
                    }

-- | Invokes its first argument with the value it gets from the source, if there is any to get.
getWith :: forall m a d x. (Monad m, AncestorFunctor a d) => (x -> Coroutine d m ()) -> Source m a x -> Coroutine d m ()
getWith consumer source = get source >>= maybe (return ()) consumer

-- | 'pour' copies all data from the /source/ argument into the /sink/ argument.
pour :: forall m a1 a2 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
        => Source m a1 x -> Sink m a2 x -> Coroutine d m ()
pour source sink = mapMStream_ (put sink) source

-- | 'mapStream' is like 'pour' that applies the function /f/ to each argument before passing it into the /sink/.
mapStream :: forall m a1 a2 d x y . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
           => (x -> y) -> Source m a1 x -> Sink m a2 y -> Coroutine d m ()
mapStream f source sink = mapMStream_ (put sink . f) source

-- | An equivalent of 'Data.List.map' that works on a 'Source' instead of a list. The argument function is applied to
-- every value after it's read from the source argument.
mapSource :: forall m a x y. Monad m => (x -> y) -> Source m a x -> Source m a y
mapSource f source = Source{get= liftM (fmap f) (get source)}

-- | An equivalent of 'Data.List.map' that works on a 'Sink' instead of a list. The argument function is applied to
-- every value vefore it's written to the sink argument.
mapSink :: forall m a x y. Monad m => (x -> y) -> Sink m a y -> Sink m a x
mapSink f sink = Sink{put= put sink . f}

-- | 'mapMaybeStream' is to 'mapStream' like 'Data.Maybe.mapMaybe' is to 'Data.List.map'.
mapMaybeStream :: forall m a1 a2 d x y . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
                => (x -> Maybe y) -> Source m a1 x -> Sink m a2 y -> Coroutine d m ()
mapMaybeStream f source sink = mapMStream_ (maybe (return ()) (put sink) . f) source

-- | 'mapMaybeSink' is to 'mapSink' like 'Data.Maybe.mapMaybe' is to 'Data.List.map'.
mapMaybeSink :: forall m a x y . Monad m => (x -> Maybe y) -> Sink m a y -> Sink m a x
mapMaybeSink f sink = Sink{put= maybe (return ()) (put sink) . f}

-- | 'mapMaybeSource' is to 'mapSource' like 'Data.Maybe.mapMaybe' is to 'Data.List.map'.
mapMaybeSource :: forall m a x y . Monad m => (x -> Maybe y) -> Source m a x -> Source m a y
mapMaybeSource f source = Source{get= next}
   where next :: forall d. AncestorFunctor a d => Coroutine d m (Maybe y)
         next = get source
                >>= maybe (return Nothing) (maybe next (return . Just) . f)

-- | 'mapMStream' is similar to 'Control.Monad.mapM'. It draws the values from a 'Source' instead of a list, writes the
-- mapped values to a 'Sink', and returns a 'Coroutine'.
mapMStream :: forall m a1 a2 d x y . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
              => (x -> Coroutine d m y) -> Source m a1 x -> Sink m a2 y -> Coroutine d m ()
mapMStream f source sink = loop
   where loop = getWith (\x-> f x >>= put sink >> loop) source

-- | An equivalent of 'Control.Monad.mapM' that works on a 'Source' instead of a list. Similar to 'mapSource', except
-- the function argument is monadic and may have perform effects.
mapMSource :: forall m a x y. Monad m
              => (forall d. AncestorFunctor a d => x -> Coroutine d m y) -> Source m a x -> Source m a y
mapMSource f source = Source{get= get source >>= maybe (return Nothing) (liftM Just . f)}

-- | An equivalent of 'Control.Monad.mapM' that works on a 'Sink' instead of a list. Similar to 'mapSink', except the
-- function argument is monadic and may have perform effects.
mapMSink :: forall m a x y. Monad m
            => (forall d. AncestorFunctor a d => x -> Coroutine d m y) -> Sink m a y -> Sink m a x
mapMSink f sink = Sink{put= (put sink =<<) . f}

-- | 'mapMStream_' is similar to 'Control.Monad.mapM_' except it draws the values from a 'Source' instead of a list and
-- works with 'Coroutine' instead of an arbitrary monad.
mapMStream_ :: forall m a d x . (Monad m, AncestorFunctor a d)
              => (x -> Coroutine d m ()) -> Source m a x -> Coroutine d m ()
mapMStream_ f source = loop
   where loop = getWith (\x-> f x >> loop) source

-- | An equivalent of 'Control.Monad.filterM'. Draws the values from a 'Source' instead of a list, writes the filtered
-- values to a 'Sink', and returns a 'Coroutine'.
filterMStream :: forall m a1 a2 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
              => (x -> Coroutine d m Bool) -> Source m a1 x -> Sink m a2 x -> Coroutine d m ()
filterMStream f source sink = mapMStream_ (\x-> f x >>= cond (put sink x) (return ())) source

-- | An equivalent of 'Control.Monad.filterM'; filters a 'Source' instead of a list.
filterMSource :: forall m a x y . Monad m
                 => (forall d. AncestorFunctor a d => x -> Coroutine d m Bool) -> Source m a x -> Source m a x
filterMSource f source = Source{get= find}
   where find :: forall d. AncestorFunctor a d => Coroutine d m (Maybe x)
         find = get source >>= maybe (return Nothing) (\x-> f x >>= cond (return (Just x)) find)

-- | An equivalent of 'Control.Monad.filterM'; filters a 'Sink' instead of a list.
filterMSink :: forall m a x y . Monad m
               => (forall d. AncestorFunctor a d => x -> Coroutine d m Bool) -> Sink m a x -> Sink m a x
filterMSink f sink = Sink{put= \x-> f x >>= cond (put sink x) (return ())}

-- | Similar to 'Data.List.foldl', but reads the values from a 'Source' instead of a list.
foldStream :: forall m a d x acc . (Monad m, AncestorFunctor a d)
              => (acc -> x -> acc) -> acc -> Source m a x -> Coroutine d m acc
foldStream f s source = loop s
   where loop s = get source >>= maybe (return s) (\x-> loop (f s x))

-- | 'foldMStream' is similar to 'Control.Monad.foldM' except it draws the values from a 'Source' instead of a list and
-- works with 'Coroutine' instead of an arbitrary monad.
foldMStream :: forall m a d x acc . (Monad m, AncestorFunctor a d)
              => (acc -> x -> Coroutine d m acc) -> acc -> Source m a x -> Coroutine d m acc
foldMStream f acc source = loop acc
   where loop acc = get source >>= maybe (return acc) (\x-> f acc x >>= loop)

-- | A version of 'foldMStream' that ignores the final result value.
foldMStream_ :: forall m a d x acc . (Monad m, AncestorFunctor a d)
                => (acc -> x -> Coroutine d m acc) -> acc -> Source m a x -> Coroutine d m ()
foldMStream_ f acc source = loop acc
   where loop acc = getWith (\x-> f acc x >>= loop) source

-- | 'unfoldMStream' is a version of 'Data.List.unfoldr' that writes the generated values into a 'Sink' instead of
-- returning a list.
unfoldMStream :: forall m a d x acc . (Monad m, AncestorFunctor a d)
                 => (acc -> Coroutine d m (Maybe (x, acc))) -> acc -> Sink m a x -> Coroutine d m acc
unfoldMStream f acc sink = loop acc
   where loop acc = f acc >>= maybe (return acc) (\(x, acc')-> put sink x >> loop acc')

-- | 'unmapMStream_' is opposite of 'mapMStream_'; it takes a 'Sink' instead of a 'Source' argument and writes the
-- generated values into it.
unmapMStream_ :: forall m a d x . (Monad m, AncestorFunctor a d)
                 => Coroutine d m (Maybe x) -> Sink m a x -> Coroutine d m ()
unmapMStream_ f sink = loop
   where loop = f >>= maybe (return ()) (\x-> put sink x >> loop)

-- | 'mapAccumStream' is similar to 'Data.List.mapAccumL' except it reads the values from a 'Source' instead of a list
-- and writes the mapped values into a 'Sink' instead of returning another list.
mapAccumStream :: forall m a1 a2 d x y acc . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d)
                  => (acc -> x -> (acc, y)) -> acc -> Source m a1 x -> Sink m a2 y -> Coroutine d m acc
mapAccumStream f acc source sink = loop acc
   where loop acc = get source >>= maybe (return acc) (\x-> let (acc', y) = f acc x in put sink y >> loop acc')

-- | Equivalent to 'Data.List.partition'. Takes a 'Source' instead of a list argument and partitions its contents into
-- the two 'Sink' arguments.
partitionStream :: forall m a1 a2 a3 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
                   => (x -> Bool) -> Source m a1 x -> Sink m a2 x -> Sink m a3 x -> Coroutine d m ()
partitionStream f source true false = mapMStream_ (\x-> if f x then put true x else put false x) source

-- | 'zipWithMStream' is similar to 'Control.Monad.zipWithM' except it draws the values from two 'Source' arguments
-- instead of two lists, sends the results into a 'Sink', and works with 'Coroutine' instead of an arbitrary monad.
zipWithMStream :: forall m a1 a2 a3 d x y z. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
                  => (x -> y -> Coroutine d m z) -> Source m a1 x -> Source m a2 y -> Sink m a3 z -> Coroutine d m ()
zipWithMStream f source1 source2 sink = loop
   where loop = do mx <- get source1
                   my <- get source2
                   case (mx, my) of (Just x, Just y) -> f x y >>= put sink >> loop
                                    _ -> return ()

-- | 'parZipWithMStream' is equivalent to 'zipWithMStream', but it consumes the two sources in parallel.
parZipWithMStream :: forall m a1 a2 a3 d x y z.
                     (MonadParallel m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
                     => (x -> y -> Coroutine d m z) -> Source m a1 x -> Source m a2 y -> Sink m a3 z -> Coroutine d m ()
parZipWithMStream f source1 source2 sink = loop
   where loop = bindM2 zip (get source1) (get source2)
         zip (Just x) (Just y) = f x y >>= put sink >> loop
         zip _ _ = return ()

-- | 'tee' is similar to 'pour' except it distributes every input value from its source argument into its both sink
-- arguments.
tee :: forall m a1 a2 a3 d x . (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d)
       => Source m a1 x -> Sink m a2 x -> Sink m a3 x -> Coroutine d m ()
tee source sink1 sink2 = distribute
   where distribute = get source >>= maybe (return ()) (\x-> put sink1 x >> put sink2 x >> distribute)

-- | Every value 'put' into a 'teeSink' result sink goes into its both argument sinks: @put (teeSink s1 s2) x@ is
-- equivalent to @put s1 x >> put s2 x@.
teeSink :: forall m a1 a2 a3 x . (Monad m, AncestorFunctor a1 a3, AncestorFunctor a2 a3)
           => Sink m a1 x -> Sink m a2 x -> Sink m a3 x
teeSink s1 s2 = Sink{put= tee}
   where tee :: forall d. AncestorFunctor a3 d => x -> Coroutine d m ()
         tee x = put s1' x >> put s2' x
         s1' :: Sink m a3 x
         s1' = liftSink s1
         s2' :: Sink m a3 x
         s2' = liftSink s2

-- | The 'Source' returned by 'teeSource' writes every value read from its argument source into the argument sink before
-- providing it back.
teeSource :: forall m a1 a2 a3 x . (Monad m, AncestorFunctor a1 a3, AncestorFunctor a2 a3)
             => Sink m a1 x -> Source m a2 x -> Source m a3 x
teeSource sink source = Source{get= tee}
   where tee :: forall d. AncestorFunctor a3 d => Coroutine d m (Maybe x)
         tee = do mx <- get source'
                  maybe (return ()) (put sink') mx
                  return mx
         sink' :: Sink m a3 x
         sink' = liftSink sink
         source' :: Source m a3 x
         source' = liftSource source

-- | 'putList' puts entire list into its /sink/ argument.
putList :: forall m a d x. (Monad m, AncestorFunctor a d) => [x] -> Sink m a x -> Coroutine d m ()
putList [] sink = return ()
putList l@(x:rest) sink = put sink x >> putList rest sink

-- | 'getList' returns the list of all values generated by the source.
getList :: forall m a d x. (Monad m, AncestorFunctor a d) => Source m a x -> Coroutine d m [x]
getList source = getList' return
   where getList' f = get source >>= maybe (f []) (\x-> getList' (f . (x:)))

-- | A utility function wrapping if-then-else, useful for handling monadic truth values
cond :: a -> a -> Bool -> a
cond x y test = if test then x else y

-- | Like 'putList', except it puts the contents of the given 'Data.Sequence.Seq' into the sink.
putQueue :: forall m a d x. (Monad m, AncestorFunctor a d) => Seq x -> Sink m a x -> Coroutine d m ()
putQueue q sink = putList (toList (viewl q)) sink
