{- 
    Copyright 2009-2010 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | This module defines various 'Control.Concurrent.SCC.Coroutine' types that operate on
-- 'Control.Concurrent.SCC.Streams.Sink' and 'Control.Concurrent.SCC.Streams.Source' values. The simplest of the bunch
-- are 'Consumer' and 'Producer' types, which respectively operate on a single source or sink. A 'Transducer' has access
-- both to a 'Control.Concurrent.SCC.Streams.Source' to read from and a 'Control.Concurrent.SCC.Streams.Sink' to write
-- into. Finally, a 'Splitter' reads from a single source and writes all input into two sinks of the same type,
-- signalling interesting input boundaries by writing into the third sink.
-- 

{-# LANGUAGE ScopedTypeVariables, KindSignatures, RankNTypes, ExistentialQuantification,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

module Control.Concurrent.SCC.Types
   (-- * Types
    Performer(..),
    OpenConsumer, Consumer(..), OpenProducer, Producer(..),
    OpenTransducer, Transducer(..), OpenSplitter, Splitter(..),
    Boundary(..), Markup(..), Parser,
    -- * Type classes
    Branching (combineBranches), 
    -- * Constructors
    isolateConsumer, isolateProducer, isolateTransducer, isolateSplitter,
    oneToOneTransducer, statelessTransducer, statefulTransducer,
    statelessSplitter, statefulSplitter,
    -- * Utility functions
    splitToConsumers, splitInputToConsumers, pipePS, (>|>), (<|<)
   )
where

import Control.Monad.Coroutine
import Control.Monad.Parallel (MonadParallel(..))

import Control.Concurrent.SCC.Streams

import Control.Category (Category(..))
import Control.Monad (liftM, when)
import Data.Maybe (maybe)

type OpenConsumer m a d x r = AncestorFunctor a d => Source m a x -> Coroutine d m r
type OpenProducer m a d x r = AncestorFunctor a d => Sink m a x -> Coroutine d m r
type OpenTransducer m a1 a2 d x y r = 
   (AncestorFunctor a1 d, AncestorFunctor a2 d) => Source m a1 x -> Sink m a2 y -> Coroutine d m r
type OpenSplitter m a1 a2 a3 a4 d x b r =
   (AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d, AncestorFunctor a4 d) =>
   Source m a1 x -> Sink m a2 x -> Sink m a3 x -> Sink m a4 b -> Coroutine d m r

-- | A coroutine that has no inputs nor outputs - and therefore may not suspend at all, which means it's not really a
-- /co/routine.
newtype Performer m r = Performer {perform :: m r}

-- | A coroutine that consumes values from a 'Control.Concurrent.SCC.Streams.Source'.
newtype Consumer m x r = Consumer {consume :: forall a d. OpenConsumer m a d x r}

-- | A coroutine that produces values and puts them into a 'Control.Concurrent.SCC.Streams.Sink'.
newtype Producer m x r = Producer {produce :: forall a d. OpenProducer m a d x r}

-- | The 'Transducer' type represents coroutines that transform a data stream.  Execution of 'transduce' must continue
-- consuming the given 'Control.Concurrent.SCC.Streams.Source' and feeding the 'Control.Concurrent.SCC.Streams.Sink' as
-- long as there is any data in the source.
newtype Transducer m x y = Transducer {transduce :: forall a1 a2 d. OpenTransducer m a1 a2 d x y ()}

-- | The 'Splitter' type represents coroutines that distribute the input stream acording to some criteria. A splitter
-- should distribute only the original input data, and feed it into the sinks in the same order it has been read from
-- the source. Furthermore, the input source should be entirely consumed and fed into the first two sinks. The third
-- sink can be used to supply extra information at arbitrary points in the input.
-- 
-- A splitter can be used in two ways: as a predicate to determine which portions of its input stream satisfy a certain
-- property, or as a chunker to divide the input stream into chunks. In the former case, the predicate is considered
-- true for exactly those parts of the input that are written to its /true/ sink. In the latter case, a chunk is a
-- contiguous section of the input stream that is written exclusively to one sink, either true or false. Anything
-- written to the third sink also terminates the chunk.
newtype Splitter m x b = Splitter {split :: forall a1 a2 a3 a4 d. OpenSplitter m a1 a2 a3 a4 d x b ()}

-- | A 'Markup' value is produced to mark either a 'Start' and 'End' of a region of data, or an arbitrary
-- 'Point' in data. A 'Point' is semantically equivalent to a 'Start' immediately followed by 'End'. The 'Content'
-- constructor wraps the actual data.
data Boundary y = Start y | End y | Point y deriving (Eq, Show)
data Markup y x = Content x | Markup (Boundary y) deriving (Eq)
type Parser m x b = Transducer m x (Markup b x)

instance Functor Boundary where
   fmap f (Start b) = Start (f b)
   fmap f (End b) = End (f b)
   fmap f (Point b) = Point (f b)

instance Functor (Markup y) where
   fmap f (Content x) = Content (f x)
   fmap f (Markup b) = Markup b

instance (Show y) => Show (Markup y Char) where
   showsPrec p (Content x) s = x : s
   showsPrec p (Markup b) s = '[' : shows b (']' : s)

instance Monad m => Category (Transducer m) where
   id = Transducer pour
   t1 . t2 = isolateTransducer $ \source sink-> 
             pipe (transduce t2 source) (\source-> transduce t1 source sink)
             >> return ()

-- | Same as 'Control.Category.>>>' except it runs the two transducers in parallel.
(>|>) :: MonadParallel m => Transducer m x y -> Transducer m y z -> Transducer m x z
t1 >|> t2 = isolateTransducer $ \source sink-> 
            pipeP (transduce t1 source) (\source-> transduce t2 source sink)
            >> return ()

-- | Same as 'Control.Category.<<<' except it runs the two transducers in parallel.
(<|<) :: MonadParallel m => Transducer m y z -> Transducer m x y -> Transducer m x z
t1 <|< t2 = isolateTransducer $ \source sink-> 
            pipeP (transduce t2 source) (\source-> transduce t1 source sink)
            >> return ()

-- | Creates a proper 'Consumer' from a function that is, but can't be proven to be, an 'OpenConsumer'.
isolateConsumer :: forall m x r. Monad m => (forall d. Functor d => Source m d x -> Coroutine d m r) -> Consumer m x r
isolateConsumer consume = Consumer consume'
   where consume' :: forall a d. OpenConsumer m a d x r
         consume' source = let source' :: Source m d x
                               source' = liftSource source
                           in consume source'

-- | Creates a proper 'Producer' from a function that is, but can't be proven to be, an 'OpenProducer'.
isolateProducer :: forall m x r. Monad m => (forall d. Functor d => Sink m d x -> Coroutine d m r) -> Producer m x r
isolateProducer produce = Producer produce'
   where produce' :: forall a d. OpenProducer m a d x r
         produce' sink = let sink' :: Sink m d x
                             sink' = liftSink sink
                         in produce sink'

-- | Creates a proper 'Transducer' from a function that is, but can't be proven to be, an 'OpenTransducer'.
isolateTransducer :: forall m x y. Monad m => 
                     (forall d. Functor d => Source m d x -> Sink m d y -> Coroutine d m ()) -> Transducer m x y
isolateTransducer transduce = Transducer transduce'
   where transduce' :: forall a1 a2 d. OpenTransducer m a1 a2 d x y ()
         transduce' source sink = let source' :: Source m d x
                                      source' = liftSource source
                                      sink' :: Sink m d y
                                      sink' = liftSink sink
                                  in transduce source' sink'

-- | Creates a proper 'Splitter' from a function that is, but can't be proven to be, an 'OpenSplitter'.
isolateSplitter :: forall m x b. Monad m => 
                   (forall d. Functor d => 
                    Source m d x -> Sink m d x -> Sink m d x -> Sink m d b -> Coroutine d m ()) 
                   -> Splitter m x b
isolateSplitter split = Splitter split'
   where split' :: forall a1 a2 a3 a4 d. OpenSplitter m a1 a2 a3 a4 d x b ()
         split' source true false edge = let source' :: Source m d x
                                             source' = liftSource source
                                             true' :: Sink m d x
                                             true' = liftSink true
                                             false' :: Sink m d x
                                             false' = liftSink false
                                             edge' :: Sink m d b
                                             edge' = liftSink edge
                                         in split source' true' false' edge'

-- | 'Branching' is a type class representing all types that can act as consumers, namely 'Consumer',
-- 'Transducer', and 'Splitter'.
class Branching c (m :: * -> *) x r | c -> m x where
   -- | 'combineBranches' is used to combine two values of 'Branch' class into one, using the given 'Consumer' binary
   -- combinator.
   combineBranches :: (forall d. (Bool ->
                                  (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x r) ->
                                  (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x r) ->
                                  (forall a. OpenConsumer m a d x r))) ->
                      Bool -> c -> c -> c

instance forall m x r. Monad m => Branching (Consumer m x r) m x r where
   combineBranches combinator parallel c1 c2 = Consumer $ combinator parallel (consume c1) (consume c2)

instance forall m x y. Monad m => Branching (Transducer m x y) m x () where
   combineBranches combinator parallel t1 t2
      = let transduce' :: forall a1 a2 d. OpenTransducer m a1 a2 d x y ()
            transduce' source sink = combinator parallel
                                        (\source-> transduce t1 source sink')
                                        (\source-> transduce t2 source sink')
                                        source
               where sink' :: Sink m d y
                     sink' = liftSink sink
        in Transducer transduce'

instance forall m x b. (MonadParallel m) => Branching (Splitter m x b) m x () where
   combineBranches combinator parallel s1 s2
      = let split' :: forall a1 a2 a3 a4 d. OpenSplitter m a1 a2 a3 a4 d x b ()
            split' source true false edge = combinator parallel
                                               (\source-> split s1 source true' false' edge')
                                               (\source-> split s2 source true' false' edge')
                                               source
               where true' :: Sink m d x
                     true' = liftSink true
                     false' :: Sink m d x
                     false' = liftSink false
                     edge' :: Sink m d b
                     edge' = liftSink edge
        in Splitter split'

-- | Function 'oneToOneTransducer' takes a function that maps one input value to one output value each, and lifts it
-- into a 'Transducer'.
oneToOneTransducer :: Monad m => (x -> y) -> Transducer m x y
oneToOneTransducer f = Transducer (mapStream f)

-- | Function 'statelessTransducer' takes a function that maps one input value into a list of output values, and
-- lifts it into a 'Transducer'.
statelessTransducer :: Monad m => (x -> [y]) -> Transducer m x y
statelessTransducer f = Transducer (\source sink-> mapMStream_ (\x-> putList (f x) sink) source)

-- | Function 'statefulTransducer' constructs a 'Transducer' from a state-transition function and the initial
-- state. The transition function may produce arbitrary output at any transition step.
statefulTransducer :: Monad m => (state -> x -> (state, [y])) -> state -> Transducer m x y
statefulTransducer f s0 = 
   Transducer (\source sink-> foldMStream_ (\ s x -> let (s', ys) = f s x in putList ys sink >> return s') s0 source)

-- | Function 'statelessSplitter' takes a function that assigns a Boolean value to each input item and lifts it into
-- a 'Splitter'.
statelessSplitter :: Monad m => (x -> Bool) -> Splitter m x b
statelessSplitter f = Splitter (\source true false edge-> partitionStream f source true false)

-- | Function 'statefulSplitter' takes a state-converting function that also assigns a Boolean value to each input
-- item and lifts it into a 'Splitter'.
statefulSplitter :: Monad m => (state -> x -> (state, Bool)) -> state -> Splitter m x ()
statefulSplitter f s0 = 
   Splitter (\source true false edge-> 
              foldMStream_ 
                 (\ s x -> let (s', truth) = f s x in (if truth then put true x else put false x) >> return s')
                 s0 source)

-- | Given a 'Splitter', a 'Source', and three consumer functions, 'splitToConsumers' runs the splitter on the source
-- and feeds the splitter's outputs to its /true/, /false/, and /edge/ sinks, respectively, to the three consumers.
splitToConsumers :: (Functor d, Monad m, d1 ~ SinkFunctor d x, AncestorFunctor a (SinkFunctor (SinkFunctor d1 x) b)) =>
                    Splitter m x b ->
                    Source m a x ->
                    (Source m (SourceFunctor d x) x -> Coroutine (SourceFunctor d x) m r1) ->
                    (Source m (SourceFunctor d1 x) x -> Coroutine (SourceFunctor d1 x) m r2) ->
                    (Source m (SourceFunctor (SinkFunctor d1 x) b) b
                     -> Coroutine (SourceFunctor (SinkFunctor d1 x) b) m r3) ->
                    Coroutine d m ((), r1, r2, r3)
splitToConsumers s source trueConsumer falseConsumer edgeConsumer
   = pipe
        (\true-> pipe
                    (\false-> pipe
                                 (split s source true false)
                                 edgeConsumer)
                    falseConsumer)
        trueConsumer
     >>= \(((extra, r3), r2), r1)-> return (extra, r1, r2, r3)

-- | Given a 'Splitter', a 'Source', and two consumer functions, 'splitInputToConsumers' runs the splitter on the source
-- and feeds the splitter's /true/ and /false/ outputs, respectively, to the two consumers.
splitInputToConsumers :: forall m a d d1 x b. (MonadParallel m, d1 ~ SinkFunctor d x, AncestorFunctor a d) =>
                         Bool -> Splitter m x b -> Source m a x ->
                         (Source m (SourceFunctor d1 x) x -> Coroutine (SourceFunctor d1 x) m ()) ->
                         (Source m (SourceFunctor d x) x -> Coroutine (SourceFunctor d x) m ()) ->
                         Coroutine d m ()
splitInputToConsumers parallel s source trueConsumer falseConsumer
   = pipePS parallel
        (\false-> pipePS parallel
                     (\true-> split s source' true false (nullSink :: Sink m d b))
                     trueConsumer)
        falseConsumer
     >> return ()
   where source' :: Source m d x
         source' = liftSource source
