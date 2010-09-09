{- 
    Copyright 2008-2010 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ScopedTypeVariables, Rank2Types, KindSignatures, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

-- | The "Combinators" module defines combinators applicable to values of the 'Transducer' and 'Splitter' types defined
-- in the "Control.Concurrent.SCC.Types" module.

module Control.Concurrent.SCC.Combinators
   (-- * Consumer, producer, and transducer combinators
    splitterToMarker,
    consumeBy, prepend, append, substitute,
    PipeableComponentPair (compose), JoinableComponentPair (join, sequence),
    -- * Pseudo-logic splitter combinators
    -- | Combinators 'sAnd' and 'sOr' are only /pseudo/-logic. While the laws of double negation and De Morgan's laws
    -- hold, 'sAnd' and 'sOr' are in general not commutative, associative, nor idempotent. In the special case when all
    -- argument splitters are stateless, such as those produced by 'Control.Concurrent.SCC.Types.statelessSplitter',
    -- these combinators do satisfy all laws of Boolean algebra.
    sNot, sAnd, sOr,
    -- ** Zipping logic combinators
    -- | The 'pAnd' and 'pOr' combinators run the argument splitters in parallel and combine their logical outputs using
    -- the corresponding logical operation on each output pair, in a manner similar to 'Data.List.zipWith'. They fully
    -- satisfy the laws of Boolean algebra.
    pAnd, pOr,
    -- * Flow-control combinators
    -- | The following combinators resemble the common flow-control programming language constructs. Combinators 
    -- 'wherever', 'unless', and 'select' are just the special cases of the combinator 'ifs'.
    --
    --    * /transducer/ ``wherever`` /splitter/ = 'ifs' /splitter/ /transducer/ 'Control.Category.id'
    --
    --    * /transducer/ ``unless`` /splitter/ = 'ifs' /splitter/ 'Control.Category.id' /transducer/
    --
    --    * 'select' /splitter/ = 'ifs' /splitter/ 'Control.Category.id'
    --    'Control.Concurrent.SCC.Primitives.suppress'
    --
    ifs, wherever, unless, select,
    -- ** Recursive
    while, nestedIn,
    -- * Section-based combinators
    -- | All combinators in this section use their 'Control.Concurrent.SCC.Splitter' argument to determine the structure
    -- of the input. Every contiguous portion of the input that gets passed to one or the other sink of the splitter is
    -- treated as one section in the logical structure of the input stream. What is done with the section depends on the
    -- combinator, but the sections, and therefore the logical structure of the input stream, are determined by the
    -- argument splitter alone.
    foreach, having, havingOnly, followedBy, even,
    -- ** first and its variants
    first, uptoFirst, prefix,
    -- ** last and its variants
    last, lastAndAfter, suffix,
    -- ** positional splitters
    startOf, endOf,
    -- ** input ranges
    between,
    -- * parser support
    parseRegions, parseNestedRegions,
    -- * helper functions
    groupMarks, findsTrueIn, findsFalseIn, teeConsumers)
where

import Control.Monad.Coroutine
import Control.Monad.Parallel (MonadParallel(..))

import Control.Concurrent.SCC.Streams
import Control.Concurrent.SCC.Types

import Prelude hiding (even, last, sequence)
import Control.Category ((>>>))
import Control.Monad (liftM, when)
import qualified Control.Monad as Monad
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (><), ViewL (EmptyL, (:<)))

import qualified Control.Category
import qualified Data.List

-- | Converts a 'Consumer' into a 'Transducer' with no output.
consumeBy :: forall m x y r. (Monad m) => Consumer m x r -> Transducer m x y
consumeBy c = Transducer $ \ source _sink -> consume c source >> return ()

-- | Class 'PipeableComponentPair' applies to any two components that can be combined into a third component with the
-- following properties:
--
--    * The input of the result, if any, becomes the input of the first component.
--
--    * The output produced by the first child component is consumed by the second child component.
--
--    * The result output, if any, is the output of the second component.
class PipeableComponentPair (m :: * -> *) w c1 c2 c3 | c1 c2 -> c3, c1 c3 -> c2, c2 c3 -> c2,
                                                       c1 -> m w, c2 -> m w, c3 -> m
   where compose :: Bool -> c1 -> c2 -> c3

instance forall m x. (MonadParallel m) =>
   PipeableComponentPair m x (Producer m x ()) (Consumer m x ()) (Performer m ())
   where compose parallel p c = let performPipe :: Coroutine Naught m ((), ())
                                    performPipe = pipePS parallel (produce p) (consume c)
                                in Performer (runCoroutine performPipe >> return ())

instance (MonadParallel m)
   => PipeableComponentPair m y (Transducer m x y) (Consumer m y r) (Consumer m x r)
   where compose parallel t c = isolateConsumer $ \source-> 
                                liftM snd $
                                pipePS parallel
                                   (transduce t source)
                                   (consume c)

instance (MonadParallel m) => PipeableComponentPair m x (Producer m x r) (Transducer m x y) (Producer m y r)
   where compose parallel p t = isolateProducer $ \sink-> 
                                liftM fst $
                                pipePS parallel
                                   (produce p)
                                   (\source-> transduce t source sink)

instance MonadParallel m => PipeableComponentPair m y (Transducer m x y) (Transducer m y z) (Transducer m x z)
   where compose parallel t1 t2 = if parallel then t1 >|> t2 else t1 >>> t2

class CompatibleSignature c cons (m :: * -> *) input output | c -> cons m

class AnyListOrUnit c

instance AnyListOrUnit [x]
instance AnyListOrUnit ()

instance (AnyListOrUnit x, AnyListOrUnit y) => CompatibleSignature (Performer m r)    (PerformerType r)  m x y
instance AnyListOrUnit y                    => CompatibleSignature (Consumer m x r)   (ConsumerType r)   m [x] y
instance AnyListOrUnit y                    => CompatibleSignature (Producer m x r)   (ProducerType r)   m y [x]
instance                                       CompatibleSignature (Transducer m x y)  TransducerType    m [x] [y]

data PerformerType r
data ConsumerType r
data ProducerType r
data TransducerType

-- | Class 'JoinableComponentPair' applies to any two components that can be combined into a third component with the
-- following properties:
--
--    * if both argument components consume input, the input of the combined component gets distributed to both
--      components in parallel,
--
--    * if both argument components produce output, the output of the combined component is a concatenation of the
--      complete output from the first component followed by the complete output of the second component, and
--
--    * the 'join' method may apply the components in any order, the 'sequence' method makes sure its first argument
--      has completed before using the second one.
class (Monad m, CompatibleSignature c1 t1 m x y, CompatibleSignature c2 t2 m x y, CompatibleSignature c3 t3 m x y)
   => JoinableComponentPair t1 t2 t3 m x y c1 c2 c3 | c1 c2 -> c3, c1 -> t1 m, c2 -> t2 m, c3 -> t3 m x y,
                                                      t1 m x y -> c1, t2 m x y -> c2, t3 m x y -> c3
   where join :: Bool -> c1 -> c2 -> c3
         sequence :: c1 -> c2 -> c3
         join = const sequence

instance forall m x r1 r2. Monad m =>
   JoinableComponentPair (ProducerType r1) (ProducerType r2) (ProducerType r2) m () [x]
                         (Producer m x r1) (Producer m x r2) (Producer m x r2)
   where sequence p1 p2 = Producer $ \sink-> produce p1 sink >> produce p2 sink

instance forall m x. MonadParallel m =>
   JoinableComponentPair (ConsumerType ()) (ConsumerType ()) (ConsumerType ()) m [x] ()
                         (Consumer m x ()) (Consumer m x ()) (Consumer m x ())
   where join parallel c1 c2 = Consumer (liftM (const ()) . teeConsumers parallel (consume c1) (consume c2))
         sequence c1 c2 = Consumer $ \source->
                          teeConsumers False (consume c1) getList source
                          >>= \((), list)-> pipe (putList list) (consume c2)
                          >> return ()

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair TransducerType TransducerType TransducerType m [x] [y]
                         (Transducer m x y) (Transducer m x y) (Transducer m x y)
   where join parallel t1 t2 = isolateTransducer $ \source sink->
                               pipe
                                  (\buffer-> teeConsumers parallel
                                                (\source-> transduce t1 source sink)
                                                (\source-> transduce t2 source buffer)
                                                source)
                                  getList
                               >>= \(_, list)-> putList list sink
         sequence t1 t2 = isolateTransducer $ \source sink->
                          teeConsumers False (flip (transduce t1) sink) getList source
                          >>= \(_, list)-> pipe (putList list) (\source-> transduce t2 source sink)
                          >> return ()

instance forall m r1 r2. MonadParallel m =>
   JoinableComponentPair (PerformerType r1) (PerformerType r2) (PerformerType r2) m () ()
                         (Performer m r1) (Performer m r2) (Performer m r2)
   where join parallel p1 p2 = Performer $ if parallel
                                           then bindM2 (const return) (perform p1) (perform p2)
                                           else perform p1 >> perform p2
         sequence p1 p2 = Performer $ perform p1 >> perform p2

instance forall m x r1 r2. (MonadParallel m) =>
   JoinableComponentPair (PerformerType r1) (ProducerType r2) (ProducerType r2) m () [x]
                         (Performer m r1) (Producer m x r2) (Producer m x r2)
   where join parallel pe pr = Producer $ \sink-> if parallel
                                                  then bindM2 (const return) (lift (perform pe)) (produce pr sink)
                                                  else lift (perform pe) >> produce pr sink
         sequence pe pr = Producer $ \sink-> lift (perform pe) >> produce pr sink

instance forall m x r1 r2. (MonadParallel m) =>
   JoinableComponentPair (ProducerType r1) (PerformerType r2) (ProducerType r2) m () [x]
                         (Producer m x r1) (Performer m r2) (Producer m x r2)
   where join parallel pr pe = Producer $ \sink-> if parallel
                                                  then bindM2 (const return) (produce pr sink) (lift (perform pe))
                                                  else produce pr sink >> lift (perform pe)
         sequence pr pe = Producer $ \sink-> produce pr sink >> lift (perform pe)

instance forall m x r1 r2. (MonadParallel m) =>
   JoinableComponentPair (PerformerType r1) (ConsumerType r2) (ConsumerType r2) m [x] ()
                         (Performer m r1) (Consumer m x r2) (Consumer m x r2)
   where join parallel p c = Consumer $ \source-> if parallel
                                                  then bindM2 (const return) (lift (perform p)) (consume c source)
                                                  else lift (perform p) >> consume c source
         sequence p c = Consumer $ \source-> lift (perform p) >> consume c source

instance forall m x r1 r2. (MonadParallel m) =>
   JoinableComponentPair (ConsumerType r1) (PerformerType r2) (ConsumerType r2) m [x] ()
                         (Consumer m x r1) (Performer m r2) (Consumer m x r2)
   where join parallel c p = Consumer $ \source-> if parallel
                                                  then bindM2 (const return) (consume c source) (lift (perform p))
                                                  else consume c source >> lift (perform p)
         sequence c p = Consumer $ \source-> consume c source >> lift (perform p)

instance forall m x y r. (MonadParallel m) =>
   JoinableComponentPair (PerformerType r) TransducerType TransducerType m [x] [y]
                         (Performer m r) (Transducer m x y) (Transducer m x y)
   where join parallel p t = Transducer $ \ source sink -> if parallel
                                                           then bindM2 (const return)
                                                                   (lift (perform p)) (transduce t source sink)
                                                           else lift (perform p) >> transduce t source sink
         sequence p t = Transducer $ \ source sink -> lift (perform p) >> transduce t source sink

instance forall m x y r. (MonadParallel m)
   => JoinableComponentPair TransducerType (PerformerType r) TransducerType m [x] [y]
                            (Transducer m x y) (Performer m r) (Transducer m x y)
   where join parallel t p = Transducer $ \ source sink -> if parallel
                                                           then bindM2 (const . return)
                                                                   (transduce t source sink) (lift (perform p))
                                                           else do result <- transduce t source sink
                                                                   lift (perform p)
                                                                   return result
         sequence t p = Transducer $ \ source sink -> do result <- transduce t source sink
                                                         lift (perform p)
                                                         return result

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair (ProducerType ()) TransducerType TransducerType m [x] [y]
                         (Producer m y ()) (Transducer m x y) (Transducer m x y)
   where join parallel p t = if parallel
                             then isolateTransducer $ \source sink->
                                     do (rest, out) <- pipe
                                                          (\buffer-> bindM2 (const return)
                                                                        (produce p sink) (transduce t source buffer))
                                                          getList
                                        putList out sink
                                        return rest
                             else sequence p t
         sequence p t = Transducer $ \ source sink -> produce p sink >> transduce t source sink

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair TransducerType (ProducerType ()) TransducerType m [x] [y]
                         (Transducer m x y) (Producer m y ()) (Transducer m x y)
   where join parallel t p = if parallel
                             then isolateTransducer $ \source sink->
                                     do (rest, out) <- pipe
                                                          (\buffer-> bindM2 (const . return)
                                                                        (transduce t source sink)
                                                                        (produce p buffer))
                                                          getList
                                        putList out sink
                                        return rest 
                             else sequence t p
         sequence t p = Transducer $ \ source sink -> do result <- transduce t source sink
                                                         produce p sink
                                                         return result

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair (ConsumerType ()) TransducerType TransducerType m [x] [y]
                         (Consumer m x ()) (Transducer m x y) (Transducer m x y)
   where join parallel c t = isolateTransducer $ \source sink->
                             teeConsumers parallel (consume c) (\source-> transduce t source sink) source
                             >> return ()
         sequence c t = isolateTransducer $ \source sink->
                        teeConsumers False (consume c) getList source
                        >>= \(_, list)-> pipe (putList list) (\source-> transduce t source sink)
                        >> return ()

instance forall m x y. MonadParallel m =>
   JoinableComponentPair TransducerType (ConsumerType ()) TransducerType m [x] [y]
                         (Transducer m x y) (Consumer m x ()) (Transducer m x y)
   where join parallel t c = join parallel c t
         sequence t c = isolateTransducer $ \source sink->
                        teeConsumers False (\source-> transduce t source sink) getList source
                        >>= \(_, list)-> pipe (putList list) (consume c)
                        >> return ()

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair (ProducerType ()) (ConsumerType ()) TransducerType m [x] [y]
                         (Producer m y ()) (Consumer m x ()) (Transducer m x y)
   where join parallel p c = Transducer $ \ source sink ->
                             if parallel
                             then bindM2 (\ _ _ -> return ()) (produce p sink) (consume c source)
                             else produce p sink >> consume c source
         sequence p c = Transducer $ \ source sink -> produce p sink >> consume c source

instance forall m x y. (MonadParallel m) =>
   JoinableComponentPair (ConsumerType ()) (ProducerType ()) TransducerType m [x] [y]
                         (Consumer m x ()) (Producer m y ()) (Transducer m x y)
   where join parallel c p = join parallel p c
         sequence c p = Transducer $ \ source sink -> consume c source >> produce p sink

-- | Combinator 'prepend' converts the given producer to a 'Control.Concurrent.SCC.Types.Transducer' that passes all its
-- input through unmodified, except for prepending the output of the argument producer to it. The following law holds: @
-- 'prepend' /prefix/ = 'join' ('substitute' /prefix/) 'Control.Category.id' @
prepend :: forall m x r. (Monad m) => Producer m x r -> Transducer m x x
prepend prefix = Transducer $ \ source sink -> produce prefix sink >> pour source sink

-- | Combinator 'append' converts the given producer to a 'Control.Concurrent.SCC.Types.Transducer' that passes all its
-- input through unmodified, finally appending the output of the argument producer to it. The following law holds: @
-- 'append' /suffix/ = 'join' 'Control.Category.id' ('substitute' /suffix/) @
append :: forall m x r. (Monad m) => Producer m x r -> Transducer m x x
append suffix = Transducer $ \ source sink -> pour source sink >> produce suffix sink >> return ()

-- | The 'substitute' combinator converts its argument producer to a 'Control.Concurrent.SCC.Types.Transducer' that
-- produces the same output, while consuming its entire input and ignoring it.
substitute :: forall m x y r. (Monad m) => Producer m y r -> Transducer m x y
substitute feed = Transducer $ \ source sink -> mapMStream_ (const $ return ()) source >> produce feed sink >> return ()

-- | The 'sNot' (streaming not) combinator simply reverses the outputs of the argument splitter. In other words, data
-- that the argument splitter sends to its /true/ sink goes to the /false/ sink of the result, and vice versa.
sNot :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
sNot splitter = isolateSplitter $ \ source true false edge -> suppressProducer (split splitter source false true)

-- | The 'sAnd' combinator sends the /true/ sink output of its left operand to the input of its right operand for
-- further splitting. Both operands' /false/ sinks are connected to the /false/ sink of the combined splitter, but any
-- input value to reach the /true/ sink of the combined component data must be deemed true by both splitters.
sAnd :: forall m x b1 b2. MonadParallel m => Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x (b1, b2)
sAnd parallel s1 s2 =
   isolateSplitter $ \ source true false edge ->
   liftM (fst . fst) $
   pipe
      (\edges-> pipePS parallel
                   (\true-> split s1 source true false (mapSink Left edges))
                   (\source-> split s2 source true false (mapSink Right edges)))
      (flip intersectRegions edge)

intersectRegions source sink = next Nothing Nothing
   where next lastLeft lastRight = getWith
                                      (either
                                          (flip pair lastRight . Just)
                                          (pair lastLeft . Just))
                                      source
         pair l@(Just x) r@(Just y) = put sink (x, y)
                                      >> next Nothing Nothing
         pair l r = next l r

-- | A 'sOr' combinator's input value can reach its /false/ sink only by going through both argument splitters' /false/
-- sinks.
sOr :: forall m x b1 b2. MonadParallel m =>
       Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x (Either b1 b2)
sOr parallel s1 s2 = isolateSplitter $ \ source true false edge ->
                     liftM fst $
                     pipePS parallel
                        (\false-> split s1 source true false (mapSink Left edge))
                        (\source-> split s2 source true false (mapSink Right edge))

-- | Combinator 'pAnd' is a pairwise logical conjunction of two splitters run in parallel on the same input.
pAnd :: forall m x b1 b2. MonadParallel m => Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x (b1, b2)
pAnd parallel s1 s2 = isolateSplitter $ \ source true false edge ->
                         pipePS parallel
                             (transduce (splittersToPairMarker parallel s1 s2) source)
                             (\source-> let split l r = getWith (test l r) source
                                            test l r (Left (x, t1, t2))
                                               = (if t1 && t2 then put true x else put false x)
                                                 >> split
                                                       (if t1 then l else Nothing)
                                                       (if t2 then r else Nothing)
                                            test _ Nothing (Right (Left l)) = split (Just l) Nothing
                                            test _ (Just r) (Right (Left l))
                                               = put edge (l, r) >> split (Just l) (Just r)
                                            test Nothing _ (Right (Right r)) = split Nothing (Just r)
                                            test (Just l) _ (Right (Right r))
                                               = put edge (l, r) >> split (Just l) (Just r)
                                        in split Nothing Nothing)
                         >> return ()

-- | Combinator 'pOr' is a pairwise logical disjunction of two splitters run in parallel on the same input.
pOr :: forall c m x b1 b2. MonadParallel m =>
       Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x (Either b1 b2)
pOr = zipSplittersWith (||) pour

ifs :: forall c m x b. (MonadParallel m, Branching c m x ()) => Bool -> Splitter m x b -> c -> c -> c
ifs parallel s c1 c2 = combineBranches if' parallel c1 c2
   where if' :: forall d. Bool -> (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x ()) ->
                (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x ()) ->
                forall a. OpenConsumer m a d x ()
         if' parallel c1 c2 source = splitInputToConsumers parallel s source c1 c2

wherever :: forall m x b. MonadParallel m => Bool -> Transducer m x x -> Splitter m x b -> Transducer m x x
wherever parallel t s = isolateTransducer wherever'
   where wherever' :: forall d. Functor d => Source m d x -> Sink m d x -> Coroutine d m ()
         wherever' source sink = pipePS parallel
                                    (\true-> split s source true sink (nullSink :: Sink m d b))
                                    (flip (transduce t) sink)
                                 >> return ()

unless :: forall m x b. MonadParallel m => Bool -> Transducer m x x -> Splitter m x b -> Transducer m x x
unless parallel t s = wherever parallel t (sNot s)

select :: forall m x b. Monad m => Splitter m x b -> Transducer m x x
select s = isolateTransducer $ \source sink-> suppressProducer (suppressProducer . split s source sink)

-- | Converts a splitter into a parser.
parseRegions :: forall m x b. Monad m => Splitter m x b -> Parser m x b
parseRegions s = isolateTransducer $ \source sink->
                    pipe
                       (transduce (splitterToMarker s) source)
                       (\source-> wrapRegions source sink)
                    >> return ()
   where wrapRegions source sink = let wrap Nothing (Left (x, _)) = put sink (Content x)
                                                                    >> return Nothing
                                       wrap (Just p) (Left (x, False)) = flush p
                                                                         >> put sink (Content x)
                                                                         >> return Nothing
                                       wrap (Just (b, t)) (Left (x, True)) =
                                          do Monad.unless t (put sink (Markup (Start b)))
                                             put sink (Content x)
                                             return (Just (b, True))
                                       wrap (Just p) (Right b') = flush p >> return (Just (b', False))
                                       wrap Nothing (Right b) = return (Just (b, False))
                                       flush (b, t) = put sink $ Markup $ (if t then End else Point) b
                                   in foldMStream wrap Nothing source >>= maybe (return ()) flush

-- | Converts a boundary-marking splitter into a parser.
parseNestedRegions :: forall m x b. Monad m => Splitter m x (Boundary b) -> Parser m x b
parseNestedRegions s = isolateTransducer $ \source sink->
                       split s source (mapSink Content sink) (mapSink Content sink) (mapSink Markup sink)

-- | The recursive combinator 'while' feeds the true sink of the argument splitter back to itself, modified by the
-- argument transducer. Data fed to the splitter's false sink is passed on unmodified.
while :: forall m x b. MonadParallel m => [(Bool, (Transducer m x x, Splitter m x b))] -> Transducer m x x
while ((parallel, (t, s)) : rest) = isolateTransducer while'
   where while' :: forall d. Functor d => Source m d x -> Sink m d x -> Coroutine d m ()
         while' source sink =
            pipePS parallel
               (\true-> split s source true sink (nullSink :: Sink m d b))
               (\source-> getWith
                       (\x-> liftM fst $
                             pipe
                                (\sink-> put sink x >> pour source sink)
                                (\source-> transduce while'' source sink))
                       source)
            >> return ()
         while'' = compose parallel t (while rest)

-- | The recursive combinator 'nestedIn' combines two splitters into a mutually recursive loop acting as a single
-- splitter.  The true sink of one of the argument splitters and false sink of the other become the true and false sinks
-- of the loop.  The other two sinks are bound to the other splitter's source.  The use of 'nestedIn' makes sense only
-- on hierarchically structured streams. If we gave it some input containing a flat sequence of values, and assuming
-- both component splitters are deterministic and stateless, an input value would either not loop at all or it would
-- loop forever.
nestedIn :: forall m x b. MonadParallel m => [(Bool, (Splitter m x b, Splitter m x b))] -> Splitter m x b
nestedIn ((parallel, (s1, s2)) : rest) =
   isolateSplitter $ \ source true false edge ->
   liftM fst $
      pipePS parallel
         (\false-> split s1 source true false edge)
         (\source-> pipe
                       (\true-> split s2 source true false (filterMSink (const $ return False) edge))
                       (\source-> get source
                                  >>= maybe
                                         (return ((), ()))
                                         (\x-> pipe
                                                  (\sink-> put sink x >> pour source sink)
                                                  (\source-> split (nestedIn rest) source true false edge))))

-- | The 'foreach' combinator is similar to the combinator 'ifs' in that it combines a splitter and two transducers into
-- another transducer. However, in this case the transducers are re-instantiated for each consecutive portion of the
-- input as the splitter chunks it up. Each contiguous portion of the input that the splitter sends to one of its two
-- sinks gets transducered through the appropriate argument transducer as that transducer's whole input. As soon as the
-- contiguous portion is finished, the transducer gets terminated.
foreach :: forall m x b c. (MonadParallel m, Branching c m x ()) => Bool -> Splitter m x b -> c -> c -> c
foreach parallel s c1 c2 = combineBranches foreach' parallel c1 c2
   where foreach' :: forall d. Bool -> 
                     (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x ()) ->
                     (forall a d'. AncestorFunctor d d' => OpenConsumer m a d' x ()) ->
                     forall a. OpenConsumer m a d x ()
         foreach' parallel c1 c2 source =
            liftM fst $
            pipePS parallel
               (transduce (splitterToMarker s) (liftSource source :: Source m d x))
               (\source-> groupMarks source (maybe c2 (const c1)))

-- | The 'having' combinator combines two pure splitters into a pure splitter. One splitter is used to chunk the input
-- into contiguous portions. Its /false/ sink is routed directly to the /false/ sink of the combined splitter. The
-- second splitter is instantiated and run on each portion of the input that goes to first splitter's /true/ sink. If
-- the second splitter sends any output at all to its /true/ sink, the whole input portion is passed on to the /true/
-- sink of the combined splitter, otherwise it goes to its /false/ sink.
having :: forall m x b1 b2. MonadParallel m => Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x b1
having parallel s1 s2 = isolateSplitter s
   where s source true false edge = pipePS parallel
                                       (transduce (splitterToMarker s1) source)
                                       (flip groupMarks test)
                                    >> return ()
            where test Nothing chunk = pour chunk false
                  test (Just mb) chunk = teeConsumers False getList (findsTrueIn s2) chunk
                                         >>= \(chunk, maybeFound)->
                                             if isJust maybeFound
                                             then maybe (return ()) (put edge) mb
                                                  >> putList chunk true
                                             else putList chunk false

-- | The 'havingOnly' combinator is analogous to the 'having' combinator, but it succeeds and passes each chunk of the
-- input to its /true/ sink only if the second splitter sends no part of it to its /false/ sink.
havingOnly :: forall m x b1 b2. MonadParallel m => Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x b1
havingOnly parallel s1 s2 = isolateSplitter s
   where s source true false edge = pipePS parallel
                                       (transduce (splitterToMarker s1) source)
                                       (flip groupMarks test)
                                    >> return ()
            where test Nothing chunk = pour chunk false
                  test (Just mb) chunk = teeConsumers False getList (findsFalseIn s2) chunk
                                         >>= \(chunk, anyFalse)->
                                             if anyFalse
                                             then putList chunk false
                                             else maybe (return ()) (put edge) mb
                                                  >> putList chunk true

-- | The result of combinator 'first' behaves the same as the argument splitter up to and including the first portion of
-- the input which goes into the argument's /true/ sink. All input following the first true portion goes into the
-- /false/ sink.
first :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
first splitter = wrapMarkedSplitter splitter $
                 \source true false edge->
                 let split 1 (Left (x, False)) = put false x >> return 1
                     split 1 (Left (x, True)) = put true x >> return 2
                     split 1 (Right b) = put edge b >> return 2
                     split 2 b@Right{} = return 3
                     split 2 (Left (x, True)) = put true x >> return 2
                     split 2 (Left (x, False)) = put false x >> return 3
                     split 3 (Left (x, _)) = put false x >> return 3
                     split 3 (Right _) = return 3
                 in foldMStream_ split 1 source

-- | The result of combinator 'uptoFirst' takes all input up to and including the first portion of the input which goes
-- into the argument's /true/ sink and feeds it to the result splitter's /true/ sink. All the rest of the input goes
-- into the /false/ sink. The only difference between 'first' and 'uptoFirst' combinators is in where they direct the
-- /false/ portion of the input preceding the first /true/ part.
uptoFirst :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
uptoFirst splitter = wrapMarkedSplitter splitter $
                     \source true false edge->
                     let split (Left q) (Left (x, False)) = return (Left (q |> x))
                         split (Left q) (Left (x, True)) = putQueue q true
                                                           >> put true x
                                                           >> return (Right True)
                         split (Left q) (Right b) = putQueue q true
                                                    >> put edge b
                                                    >> return (Right True)
                         split (Right True) Right{} = return (Right False)
                         split (Right True) (Left (x, True)) = put true x >> return (Right True)
                         split (Right True) (Left (x, False)) = put false x >> return (Right False)
                         split (Right False) (Left (x, _)) = put false x >> return (Right False)
                         split (Right False) (Right _) = return (Right False)
                     in foldMStream split (Left Seq.empty) source
                           >>= either (flip putQueue false) (const $ return ())

-- | The result of the combinator 'last' is a splitter which directs all input to its /false/ sink, up to the last
-- portion of the input which goes to its argument's /true/ sink. That portion of the input is the only one that goes to
-- the resulting component's /true/ sink.  The splitter returned by the combinator 'last' has to buffer the previous two
-- portions of its input, because it cannot know if a true portion of the input is the last one until it sees the end of
-- the input or another portion succeeding the previous one.
last :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
last splitter = wrapMarkedSplitter splitter $
                \source true false edge->
                let get1 (Left (x, False)) = put false x
                                             >> getWith get1 source
                    get1 p@(Left (x, True)) = get2 Nothing Seq.empty p
                    get1 (Right b) = getWith (get2 (Just b) Seq.empty) source
                    get2 mb q (Left (x, True)) = let q' = q |> x
                                                 in get source
                                                    >>= maybe
                                                           (flush mb q')
                                                           (get2 mb q')
                    get2 mb q p = get3 mb q Seq.empty p
                    get3 mb qt qf (Left (x, False)) =
                       let qf' = qf |> x
                       in get source
                          >>= maybe
                                 (flush mb qt >> putQueue qf' false)
                                 (get3 mb qt qf')
                    get3 mb qt qf p = do putQueue qt false
                                         putQueue qf false
                                         get1 p
                    flush mb q = maybe (return ()) (put edge) mb
                                 >> putQueue q true
                in getWith get1 source

-- | The result of the combinator 'lastAndAfter' is a splitter which directs all input to its /false/ sink, up to the
-- last portion of the input which goes to its argument's /true/ sink. That portion and the remainder of the input is
-- fed to the resulting component's /true/ sink. The difference between 'last' and 'lastAndAfter' combinators is where
-- they feed the /false/ portion of the input, if any, remaining after the last /true/ part.
lastAndAfter :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
lastAndAfter splitter = wrapMarkedSplitter splitter $
                        \source true false edge->
                        let get1 (Left (x, False)) = put false x
                                                     >> getWith get1 source
                            get1 p@(Left (x, True)) = get2 Nothing Seq.empty p
                            get1 (Right b) = getWith (get2 (Just b) Seq.empty) source
                            get2 mb q (Left (x, True)) = let q' = q |> x
                                                         in get source
                                                            >>= maybe
                                                                   (flush mb q')
                                                                   (get2 mb q')
                            get2 mb q p = get3 mb q p
                            get3 mb q (Left (x, False)) = let q' = q |> x
                                                          in get source
                                                             >>= maybe
                                                                    (flush mb q')
                                                                    (get3 mb q')
                            get3 _ q p@(Left (x, True)) = putQueue q false
                                                          >> get1 p
                            get3 _ q b'@Right{} = putQueue q false
                                                  >> get1 b'
                            flush mb q = maybe (return ()) (put edge) mb
                                         >> putQueue q true
                        in getWith get1 source

-- | The 'prefix' combinator feeds its /true/ sink only the prefix of the input that its argument feeds to its /true/
-- sink.  All the rest of the input is dumped into the /false/ sink of the result.
prefix :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
prefix splitter = wrapMarkedSplitter splitter $
                  \source true false edge->
                  let split 0 p@Left{} = split 1 p
                      split 0 (Right b) = put edge b >> return 1
                      split 1 (Left (x, False)) = put false x >> return 2
                      split 1 (Left (x, True)) = put true x >> return 1
                      split 1 (Right b) = return 2
                      split 2 (Left (x, _)) = put false x >> return 2
                      split 2 Right{} = return 2
                  in foldMStream_ split 0 source

-- | The 'suffix' combinator feeds its /true/ sink only the suffix of the input that its argument feeds to its /true/
-- sink.  All the rest of the input is dumped into the /false/ sink of the result.
suffix :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
suffix splitter = wrapMarkedSplitter splitter $
                  \source true false edge->
                  let split Nothing (Left (x, False)) = put false x >> return Nothing
                      split Nothing (Left (x, True)) = return (Just (Nothing, Seq.singleton x))
                      split Nothing (Right b) = return (Just (Just b, Seq.empty))
                      split (Just (mb, q)) (Left (x, True)) = return (Just (mb, q |> x))
                      split (Just (mb, q)) (Left (x, False)) = putQueue q false
                                                               >> put false x
                                                               >> return Nothing
                      split (Just (mb, q)) (Right b) = putQueue q false
                                                       >> return (Just (Just b, Seq.empty))
                  in foldMStream split Nothing source
                        >>= \r-> case r of Nothing -> return ()
                                           Just (Nothing, q) -> putQueue q true
                                           Just (Just b, q) -> put edge b >> putQueue q true

-- | The 'even' combinator takes every input section that its argument /splitter/ deems /true/, and feeds even ones into
-- its /true/ sink. The odd sections and parts of input that are /false/ according to its argument splitter are fed to
-- 'even' splitter's /false/ sink.
even :: forall m x b. Monad m => Splitter m x b -> Splitter m x b
even splitter = wrapMarkedSplitter splitter $
                \source true false edge->
                let split 1 (Left (x, False)) = put false x >> return 1
                    split 1 p@(Left (x, True)) = split 2 p
                    split 1 (Right b) = return 2
                    split 2 (Left (x, True)) = put false x >> return 2
                    split 2 p@(Left (x, False)) = split 3 p
                    split 2 (Right b) = put edge b >> return 4
                    split 3 (Left (x, False)) = put false x >> return 3
                    split 3 p@(Left (x, True)) = split 4 p
                    split 3 (Right b) = put edge b >> return 4
                    split 4 (Left (x, True)) = put true x >> return 4
                    split 4 p@(Left (x, False)) = split 1 p
                    split 4 (Right b) = return 2
                in foldMStream_ split 1 source

-- | Splitter 'startOf' issues an empty /true/ section at the beginning of every section considered /true/ by its
-- argument splitter, otherwise the entire input goes into its /false/ sink.
startOf :: forall m x b. Monad m => Splitter m x b -> Splitter m x (Maybe b)
startOf splitter = wrapMarkedSplitter splitter $
                   \source true false edge->
                   let split 1 (Left (x, False)) = put false x >> return 1
                       split 1 p@(Left (x, True)) = put edge Nothing >> split 2 p
                       split 1 (Right b) = put edge (Just b) >> return 2
                       split 2 (Left (x, True)) = put false x >> return 2
                       split 2 p = split 1 p
                   in foldMStream_ split 1 source

-- | Splitter 'endOf' issues an empty /true/ section at the end of every section considered /true/ by its argument
-- splitter, otherwise the entire input goes into its /false/ sink.
endOf :: forall m x b. Monad m => Splitter m x b -> Splitter m x (Maybe b)
endOf splitter = wrapMarkedSplitter splitter $
                 \source true false edge->
                 let split Nothing (Left (x, False)) = put false x >> return Nothing
                     split Nothing p@(Left (x, True)) = split (Just Nothing) p
                     split Nothing (Right b) = return (Just (Just b))
                     split (Just mb) (Left (x, True)) = put false x >> return (Just mb)
                     split (Just mb) p@(Left (x, False)) = put edge mb >> split Nothing p
                     split (Just mb) (Right b) = put edge mb >> return (Just $ Just b)
                 in foldMStream split Nothing source >>= maybe (return ()) (put edge)

-- | Combinator 'followedBy' treats its argument 'Splitter's as patterns components and returns a 'Splitter' that
-- matches their concatenation. A section of input is considered /true/ by the result iff its prefix is considered
-- /true/ by argument /s1/ and the rest of the section is considered /true/ by /s2/. The splitter /s2/ is started anew
-- after every section split to /true/ sink by /s1/.
followedBy :: forall m x b1 b2. MonadParallel m =>
              Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x (b1, b2)
followedBy parallel s1 s2 = 
   isolateSplitter $ \ source true false edge ->
   pipePS parallel
      (transduce (splitterToMarker s1) source)
      (\source-> let get0 q = case Seq.viewl q
                              of Seq.EmptyL -> getWith get1 source
                                 (Left (x, False)) :< rest -> put false x
                                                              >> get0 rest
                                 (Left (x, True)) :< rest -> get2 Nothing Seq.empty q
                                 (Right b) :< rest -> get2 (Just b) Seq.empty rest
                     get1 (Left (x, False)) = put false x
                                              >> getWith get1 source
                     get1 p@(Left (x, True)) = get2 Nothing Seq.empty (Seq.singleton p)
                     get1 (Right b) = get2 (Just b) Seq.empty Seq.empty
                     get2 mb q q' = case Seq.viewl q'
                                    of Seq.EmptyL -> get source
                                                     >>= maybe (testEnd mb q) (get2 mb q . Seq.singleton)
                                       (Left (x, True)) :< rest -> get2 mb (q |> x) rest
                                       (Left (x, False)) :< rest -> get3 mb q q'
                                       Right{} :< rest -> get3 mb q q'
                     get3 mb q q' = do ((q1, q2), n) <- pipe (get7 Seq.empty q') (test mb q)
                                       case n of Nothing -> putQueue q false
                                                            >> get0 (q1 >< q2)
                                                 Just 0 -> get0 (q1 >< q2)
                                                 Just n -> get8 (Just mb) n (q1 >< q2)
                     get7 q1 q2 sink = case Seq.viewl q2
                                       of Seq.EmptyL -> get source
                                                        >>= maybe (return (q1, q2))
                                                               (\p-> either
                                                                        (put sink . fst)
                                                                        (const $ return ())
                                                                        p
                                                                     >> get7 (q1 |> p) q2 sink)
                                          p :< rest -> either
                                                          (put sink . fst)
                                                          (const $ return ()) p
                                                       >> get7 (q1 |> p) rest sink
                     testEnd mb q = do ((), n) <- pipe (const $ return ()) (test mb q)
                                       case n of Nothing -> putQueue q false
                                                 _ -> return ()
                     test mb q source = liftM snd $
                                        pipe
                                           (transduce (splitterToMarker s2) source)
                                           (\source-> let get4 (Left (_, False)) = return Nothing
                                                          get4 p@(Left (_, True)) = putQueue q true
                                                                                    >> get5 0 p
                                                          get4 p@(Right b) = maybe
                                                                                (return ())
                                                                                (\b1-> put edge (b1, b))
                                                                                mb
                                                                             >> putQueue q true
                                                                             >> get6 0
                                                          get5 n (Left (x, True)) = put true x
                                                                                    >> get6 (succ n)
                                                          get5 n _ = return (Just n)
                                                          get6 n = get source
                                                                   >>= maybe
                                                                          (return $ Just n)
                                                                          (get5 n)
                                                      in get source >>= maybe (return Nothing) get4)
                     get8 Nothing 0 q = get0 q
                     get8 (Just mb) 0 q = get2 mb Seq.empty q
                     get8 mmb n q = case Seq.viewl q of Left (x, False) :< rest -> get8 Nothing (pred n) rest
                                                        Left (x, True) :< rest
                                                           -> get8 (maybe (Just Nothing) Just mmb) (pred n) rest
                                                        Right b :< rest -> get8 (Just (Just b)) n rest
                in get0 Seq.empty)
   >> return ()

-- | Combinator '...' tracks the running balance of difference between the number of preceding starts of sections
-- considered /true/ according to its first argument and the ones according to its second argument. The combinator
-- passes to /true/ all input values for which the difference balance is positive. This combinator is typically used
-- with 'startOf' and 'endOf' in order to count entire input sections and ignore their lengths.
between :: forall m x b1 b2. MonadParallel m => Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x b1
between parallel s1 s2 = isolateSplitter $ \ source true false edge ->
                         pipePS parallel
                            (transduce (splittersToPairMarker parallel s1 s2) source)
                            (let pass n x = (if n > 0 then put true x else put false x)
                                            >> return n
                                 pass' n x = (if n >= 0 then put true x else put false x)
                                             >> return n
                                 state n (Left (x, True, False)) = pass (succ n) x
                                 state n (Left (x, False, True)) = pass' (pred n) x
                                 state n (Left (x, True, True)) = pass' n x
                                 state n (Left (x, False, False)) = pass n x
                                 state 0 (Right (Left b)) = put edge b >> return 1
                                 state n (Right (Left _)) = return (succ n)
                                 state n (Right (Right _)) = return (pred n)
                             in foldMStream_ state 0)
                         >> return ()

-- Helper functions

wrapMarkedSplitter ::
   forall m x b1 b2. Monad m =>
   Splitter m x b1
   -> (forall a1 a2 a3 a4 d. (AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d, AncestorFunctor a4 d) =>
       Source m a1 (Either (x, Bool) b1) -> Sink m a2 x -> Sink m a3 x -> Sink m a4 b2 -> Coroutine d m ())
   -> Splitter m x b2
wrapMarkedSplitter splitter splitMarked = isolateSplitter $ \ source true false edge ->
                                          pipe
                                             (transduce (splitterToMarker splitter) source)
                                             (\source-> splitMarked source true false edge)
                                          >> return ()

splitterToMarker :: forall m x b. Monad m => Splitter m x b -> Transducer m x (Either (x, Bool) b)
splitterToMarker s = isolateTransducer $ \source sink->
                     split s source
                        (mapSink (\x-> Left (x, True)) sink)
                        (mapSink (\x-> Left (x, False)) sink)
                        (mapSink Right sink)

splittersToPairMarker :: forall m x b1 b2. (MonadParallel m) => Bool -> Splitter m x b1 -> Splitter m x b2 ->
                         Transducer m x (Either (x, Bool, Bool) (Either b1 b2))
splittersToPairMarker parallel s1 s2 =
   let t source sink = 
          pipe
             (\sync-> teeConsumers parallel
                         (\source1-> split s1 source1
                                        (mapSink (\x-> Left ((x, True), True)) sync)
                                        (mapSink (\x-> Left ((x, False), True)) sync)
                                        (mapSink (Right. Left) sync))
                         (\source2-> split s2 source2
                                        (mapSink (\x-> Left ((x, True), False)) sync)
                                        (mapSink (\x-> Left ((x, False), False)) sync)
                                        (mapSink (Right . Right) sync))
                         source)
              (synchronizeMarks sink)
          >> return ()
       synchronizeMarks :: forall m a1 a2 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d) =>
                           Sink m a1 (Either (x, Bool, Bool) (Either b1 b2))
                        -> Source m a2 (Either ((x, Bool), Bool) (Either b1 b2))
                        -> Coroutine d m ()
       synchronizeMarks sink source = foldMStream handleMark Nothing source >>= \Nothing-> return () where
          handleMark Nothing (Right b) = put sink (Right b) >> return Nothing
          handleMark Nothing (Left (p, first)) = return (Just (Seq.singleton (Left p), first))
          handleMark state@(Just (q, first)) (Left (p, first')) | first == first' = return (Just (q |> Left p, first))
          handleMark state@(Just (q, True)) (Right b@Left{}) = return (Just (q |> Right b, True))
          handleMark state@(Just (q, False)) (Right b@Right{}) = return (Just (q |> Right b, False))
          handleMark state (Right b) = put sink (Right b) >> return state
          handleMark state@(Just (q, pos')) mark@(Left ((x, t), pos))
             = case Seq.viewl q
               of Seq.EmptyL -> return (Just (Seq.singleton (Left (x, t)), pos))
                  Right b :< rest -> put sink (Right b)
                                     >> handleMark (if Seq.null rest then Nothing else Just (rest, pos')) mark
                  Left (y, t') :< rest -> put sink (Left $ if pos then (y, t, t') else (y, t', t))
                                          >> return (if Seq.null rest then Nothing else Just (rest, pos'))
       returnQueuedList q = return $ concatMap (either ((:[]) . fst) (const [])) $ Foldable.toList $ Seq.viewl q
   in isolateTransducer t

zipSplittersWith :: forall m x b1 b2 b. MonadParallel m => 
                    (Bool -> Bool -> Bool) -> 
                    (forall a1 a2 d. (AncestorFunctor a1 d, AncestorFunctor a2 d) =>
                     Source m a1 (Either b1 b2) -> Sink m a2 b -> Coroutine d m ()) -> 
                    Bool -> Splitter m x b1 -> Splitter m x b2 -> Splitter m x b
zipSplittersWith f boundaries parallel s1 s2
   = isolateSplitter $ \ source true false edge ->
     pipe
        (\edge->
         pipePS parallel
            (transduce (splittersToPairMarker parallel s1 s2) source)
            (mapMStream_
                (either
                    (\(x, t1, t2)-> if f t1 t2 then put true x else put false x)
                    (put edge))))
        (flip boundaries edge)
     >> return ()

-- | Runs the second argument on every contiguous region of input source (typically produced by 'splitterToMarker')
-- whose all values either match @Left (_, True)@ or @Left (_, False)@.
groupMarks :: (Monad m, AncestorFunctor a d, AncestorFunctor a (SinkFunctor d x)) =>
              Source m a (Either (x, Bool) b) ->
              (Maybe (Maybe b) -> Source m (SourceFunctor d x) x -> Coroutine (SourceFunctor d x) m r) ->
              Coroutine d m ()
groupMarks source getConsumer = start
   where start = getWith (either startContent startRegion) source
         startContent (x, False) = pipe (\sink-> pass False sink x) (getConsumer Nothing)
                                   >>= maybe (return ()) (either startContent startRegion) . fst
         startContent (x, True) = pipe (\sink-> pass True sink x) (getConsumer $ Just Nothing)
                                  >>= maybe (return ()) (either startContent startRegion) . fst
         startRegion b = pipe (next True) (getConsumer (Just $ Just b))
                         >>= maybe (return ()) (either startContent startRegion) . fst
         pass t sink x = put sink x >> next t sink
         next t sink = get source >>= maybe (return Nothing) (continue t sink)
         continue t sink (Left (x, t')) | t == t' = pass t sink x
         continue t sink p = return (Just p)

-- | 'suppressProducer' runs the /producer/ argument with a new sink, suppressing everything 'put' in the sink.
suppressProducer :: forall m a x r. (Functor a, Monad m) => (Sink m a x -> Coroutine a m r) -> Coroutine a m r
suppressProducer producer = producer (nullSink :: Sink m a x)

findsTrueIn :: forall m a d x b. (Monad m, AncestorFunctor a d)
               => Splitter m x b -> Source m a x -> Coroutine d m (Maybe (Maybe b))
findsTrueIn splitter source = pipe
                                 (\testTrue-> pipe
                                                 (split splitter (liftSource source :: Source m d x)
                                                     testTrue
                                                     (nullSink :: Sink m d x))
                                                 get)
                                 get
                              >>= \(((), maybeEdge), maybeTrue)-> return $
                                                                  case maybeEdge
                                                                  of Nothing -> fmap (const Nothing) maybeTrue
                                                                     _ -> Just maybeEdge

findsFalseIn :: forall m a d x b. (Monad m, AncestorFunctor a d) => Splitter m x b -> Source m a x -> Coroutine d m Bool
findsFalseIn splitter source = pipe
                                  (\testFalse-> split splitter (liftSource source :: Source m d x)
                                                   (nullSink :: Sink m d x)
                                                   testFalse
                                                   (nullSink :: Sink m d b))
                                  get
                               >>= \((), maybeFalse)-> return (isJust maybeFalse)

teeConsumers :: forall m a d x r1 r2. MonadParallel m
                => Bool -> (forall a. OpenConsumer m a (SinkFunctor d x) x r1)
                        -> (forall a. OpenConsumer m a (SourceFunctor d x) x r2)
             -> OpenConsumer m a d x (r1, r2)
teeConsumers parallel c1 c2 source = pipePS parallel consume1 c2
   where consume1 sink = c1 (teeSource sink source' :: Source m (SinkFunctor d x) x)
         source' :: Source m d x
         source' = liftSource source
