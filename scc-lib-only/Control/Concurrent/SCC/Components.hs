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

-- | The "Components" module defines thin wrappers around the 'Transducer' and 'Splitter' primitives and combinators,
-- relying on the "Control.Concurrent.SCC.ComponentTypes" module.

module Control.Concurrent.SCC.Components where

import Control.Monad.Coroutine
import Control.Monad.Parallel (MonadParallel(..))

import Control.Concurrent.SCC.Types
import Control.Concurrent.SCC.Types as Types
import qualified Control.Concurrent.SCC.Combinators as Combinator
import qualified Control.Concurrent.SCC.Primitives as Primitive
import qualified Control.Concurrent.SCC.XML as XML
import Control.Concurrent.SCC.Primitives (OccurenceTag)
import Control.Concurrent.SCC.XML (Token)
import Control.Concurrent.Configuration

import Prelude hiding (appendFile, even, id, last, sequence, (||), (&&))
import qualified Control.Category
import Control.Monad (liftM)

import System.IO (Handle)

-- | A component that performs a computation with no inputs nor outputs is a 'PerformerComponent'.
type PerformerComponent m r = Component (Performer m r)

-- | A component that consumes values from a 'Source' is called 'ConsumerComponent'.
type ConsumerComponent m x r = Component (Consumer m x r)

-- | A component that produces values and puts them into a 'Sink' is called 'ProducerComponent'.
type ProducerComponent m x r = Component (Producer m x r)

-- | The 'TransducerComponent' type represents computations that transform a data stream.
type TransducerComponent m x y = Component (Transducer m x y)

type ParserComponent m x y = Component (Parser m x y)

-- | The 'SplitterComponent' type represents computations that distribute data acording to some criteria.  A splitter
-- should distribute only the original input data, and feed it into the sinks in the same order it has been read from
-- the source. If the two 'Sink c x' arguments of a splitter are the same, the splitter must act as an identity
-- transform.
type SplitterComponent m x b = Component (Splitter m x b)

-- | The constant cost of each I/O-performing component.
ioCost :: Int
ioCost = 5

-- | ConsumerComponent 'toList' copies the given source into a list.
toList :: forall m x. Monad m => ConsumerComponent m x [x]
toList = atomic "toList" 1 Primitive.toList

-- | 'fromList' produces the contents of the given list argument.
fromList :: forall m x. Monad m => [x] -> ProducerComponent m x ()
fromList l = atomic "fromList" 1 (Primitive.fromList l)

-- | ConsumerComponent 'toStdOut' copies the given source into the standard output.
toStdOut :: ConsumerComponent IO Char ()
toStdOut = atomic "toStdOut" ioCost Primitive.toStdOut

-- | ProducerComponent 'fromStdIn' feeds the given sink from the standard input.
fromStdIn :: ProducerComponent IO Char ()
fromStdIn = atomic "fromStdIn" ioCost Primitive.fromStdIn

-- | ProducerComponent 'fromFile' opens the named file and feeds the given sink from its contents.
fromFile :: String -> ProducerComponent IO Char ()
fromFile path = atomic "fromFile" ioCost (Primitive.fromFile path)

-- | ProducerComponent 'fromHandle' feeds the given sink from the open file /handle/. The argument /doClose/ determines
-- | if /handle/ should be closed when the handle is consumed or the sink closed.
fromHandle :: Handle -> Bool -> ProducerComponent IO Char ()
fromHandle handle doClose = atomic "fromHandle" ioCost (Primitive.fromHandle handle doClose)

-- | ConsumerComponent 'toFile' opens the named file and copies the given source into it.
toFile :: String -> ConsumerComponent IO Char ()
toFile path = atomic "toFile" ioCost (Primitive.toFile path)

-- | ConsumerComponent 'appendFile' opens the name file and appends the given source to it.
appendFile :: String -> ConsumerComponent IO Char ()
appendFile path = atomic "appendFile" ioCost (Primitive.appendFile path)

-- | ConsumerComponent 'toHandle' copies the given source into the open file /handle/. The argument /doClose/ determines
-- | if /handle/ should be closed once the entire source is consumed and copied.
toHandle :: Handle -> Bool -> ConsumerComponent IO Char ()
toHandle handle doClose = atomic "toHandle" ioCost (Primitive.toHandle handle doClose)

-- | TransducerComponent 'id' passes its input through unmodified.
id :: forall m x. Monad m => TransducerComponent m x x
id = atomic "id" 1 Control.Category.id

-- | TransducerComponent 'unparse' removes all markup from its input and passes the content through.
unparse :: forall m x y. Monad m => TransducerComponent m (Markup y x) x
unparse = atomic "unparse" 1 Primitive.unparse

-- | TransducerComponent 'parse' prepares input content for subsequent parsing.
parse :: forall m x y. Monad m => TransducerComponent m x (Markup y x)
parse = atomic "parse" 1 Primitive.parse

-- | The 'suppress' consumer suppresses all input it receives. It is equivalent to 'substitute' []
suppress :: forall m x y. Monad m => ConsumerComponent m x ()
suppress = atomic "suppress" 1 Primitive.suppress

-- | The 'erroneous' consumer reports an error if any input reaches it.
erroneous :: forall m x. Monad m => String -> ConsumerComponent m x ()
erroneous message = atomic "erroneous" 0 (Primitive.erroneous message)

-- | The 'lowercase' transforms all uppercase letters in the input to lowercase, leaving the rest unchanged.
lowercase :: forall m. Monad m => TransducerComponent m Char Char
lowercase = atomic "lowercase" 1 Primitive.lowercase

-- | The 'uppercase' transforms all lowercase letters in the input to uppercase, leaving the rest unchanged.
uppercase :: forall m. Monad m => TransducerComponent m Char Char
uppercase = atomic "uppercase" 1 Primitive.uppercase

-- | The 'count' transducer counts all its input values and outputs the final tally.
count :: forall m x. Monad m => TransducerComponent m x Integer
count = atomic "count" 1 Primitive.count

-- | Converts each input value @x@ to @show x@.
toString :: forall m x. (Monad m, Show x) => TransducerComponent m x String
toString = atomic "toString" 1 Primitive.toString

-- | TransducerComponent 'group' collects all its input values into a single list.
group :: forall m x. Monad m => TransducerComponent m x [x]
group = atomic "group" 1 Primitive.group

-- | TransducerComponent 'concatenate' flattens the input stream of lists of values into the output stream of values.
concatenate :: forall m x. Monad m => TransducerComponent m [x] x
concatenate = atomic "concatenate" 1 Primitive.concatenate

-- | Same as 'concatenate' except it inserts the given separator list between every two input lists.
concatSeparate :: forall m x. Monad m => [x] -> TransducerComponent m [x] x
concatSeparate separator = atomic "concatSeparate" 1 (Primitive.concatSeparate separator)

-- | SplitterComponent 'whitespace' feeds all white-space characters into its /true/ sink, all others into /false/.
whitespace :: forall m. Monad m => SplitterComponent m Char ()
whitespace = atomic "whitespace" 1 Primitive.whitespace

-- | SplitterComponent 'letters' feeds all alphabetical characters into its /true/ sink, all other characters into
-- | /false/.
letters :: forall m. Monad m => SplitterComponent m Char ()
letters = atomic "letters" 1 Primitive.letters

-- | SplitterComponent 'digits' feeds all digits into its /true/ sink, all other characters into /false/.
digits :: forall m. Monad m => SplitterComponent m Char ()
digits = atomic "digits" 1 Primitive.digits

-- | SplitterComponent 'nonEmptyLine' feeds line-ends into its /false/ sink, and all other characters into /true/.
nonEmptyLine :: forall m. Monad m => SplitterComponent m Char ()
nonEmptyLine = atomic "nonEmptyLine" 1 Primitive.nonEmptyLine

-- | The sectioning splitter 'line' feeds line-ends into its /false/ sink, and line contents into /true/. A single
-- line-end can be formed by any of the character sequences \"\\n\", \"\\r\", \"\\r\\n\", or \"\\n\\r\".
line :: forall m. Monad m => SplitterComponent m Char ()
line = atomic "line" 1 Primitive.line

-- | SplitterComponent 'everything' feeds its entire input into its /true/ sink.
everything :: forall m x. Monad m => SplitterComponent m x ()
everything = atomic "everything" 1 Primitive.everything

-- | SplitterComponent 'nothing' feeds its entire input into its /false/ sink.
nothing :: forall m x. Monad m => SplitterComponent m x ()
nothing = atomic "nothing" 1 Primitive.nothing

-- | SplitterComponent 'one' feeds all input values to its /true/ sink, treating every value as a separate section.
one :: forall m x. Monad m => SplitterComponent m x ()
one = atomic "one" 1 Primitive.one

-- | SplitterComponent 'marked' passes all marked-up input sections to its /true/ sink, and all unmarked input to its
-- /false/ sink.
marked :: forall m x y. (Monad m, Eq y) => SplitterComponent m (Markup y x) ()
marked = atomic "marked" 1 Primitive.marked

-- | SplitterComponent 'markedContent' passes the content of all marked-up input sections to its /true/ sink, while the
-- outermost tags and all unmarked input go to its /false/ sink.
markedContent :: forall m x y. (Monad m, Eq y) => SplitterComponent m (Markup y x) ()
markedContent = atomic "markedContent" 1 Primitive.markedContent

-- | SplitterComponent 'markedWith' passes input sections marked-up with the appropriate tag to its /true/ sink, and the
-- rest of the input to its /false/ sink. The argument /select/ determines if the tag is appropriate.
markedWith :: forall m x y. (Monad m, Eq y) => (y -> Bool) -> SplitterComponent m (Markup y x) ()
markedWith select = atomic "markedWith" 1 (Primitive.markedWith select)

-- | SplitterComponent 'contentMarkedWith' passes the content of input sections marked-up with the appropriate tag to
-- its /true/ sink, and the rest of the input to its /false/ sink. The argument /select/ determines if the tag is
-- appropriate.
contentMarkedWith :: forall m x y. (Monad m, Eq y) => (y -> Bool) -> SplitterComponent m (Markup y x) ()
contentMarkedWith select = atomic "contentMarkedWith" 1 (Primitive.contentMarkedWith select)

-- | Performs the same task as the 'substring' splitter, but instead of splitting it outputs the input as @'Markup' x
-- 'OccurenceTag'@ in order to distinguish overlapping strings.
parseSubstring :: forall m x y. (Monad m, Eq x) => [x] -> ParserComponent m x OccurenceTag
parseSubstring list = atomic "parseSubstring" 1 (Primitive.parseSubstring list)

-- | SplitterComponent 'substring' feeds to its /true/ sink all input parts that match the contents of the given list
-- argument. If two overlapping parts of the input both match the argument, both are sent to /true/ and each is preceded
-- by an edge.
substring :: forall m x. (Monad m, Eq x) => [x] -> SplitterComponent m x ()
substring list = atomic "substring" 1 (Primitive.substring list)

-- | Converts a 'ConsumerComponent' into a 'TransducerComponent' with no output.
consumeBy :: forall m x y r. (Monad m) => ConsumerComponent m x r -> TransducerComponent m x y
consumeBy = lift 1 "consumeBy" Combinator.consumeBy

-- | Class 'PipeableComponentPair' applies to any two components that can be combined into a third component with the
-- following properties:
--
--    * The input of the result, if any, becomes the input of the first component.
--
--    * The output produced by the first child component is consumed by the second child component.
--
--    * The result output, if any, is the output of the second component.

(>->) :: Combinator.PipeableComponentPair m w c1 c2 c3 => Component c1 -> Component c2 -> Component c3
(>->) = liftParallelPair ">->" Combinator.compose

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
join :: Combinator.JoinableComponentPair t1 t2 t3 m x y c1 c2 c3 => Component c1 -> Component c2 -> Component c3
join = liftParallelPair "join" Combinator.join

sequence :: Combinator.JoinableComponentPair t1 t2 t3 m x y c1 c2 c3 => Component c1 -> Component c2 -> Component c3
sequence = liftSequentialPair "sequence" Combinator.sequence

-- | Combinator 'prepend' converts the given producer to transducer that passes all its input through unmodified, except
-- | for prepending the output of the argument producer to it.
-- | 'prepend' /prefix/ = 'join' ('substitute' /prefix/) 'asis'
prepend :: forall m x r. (Monad m) => ProducerComponent m x r -> TransducerComponent m x x
prepend = lift 1 "prepend" Combinator.prepend

-- | Combinator 'append' converts the given producer to transducer that passes all its input through unmodified, finally
-- | appending to it the output of the argument producer.
-- | 'append' /suffix/ = 'join' 'asis' ('substitute' /suffix/)
append :: forall m x r. (Monad m) => ProducerComponent m x r -> TransducerComponent m x x
append = lift 1 "append" Combinator.append

-- | The 'substitute' combinator converts its argument producer to a transducer that produces the same output, while
-- | consuming its entire input and ignoring it.
substitute :: forall m x y r. (Monad m) => ProducerComponent m y r -> TransducerComponent m x y
substitute = lift 1 "substitute" Combinator.substitute

-- | The 'snot' (streaming not) combinator simply reverses the outputs of the argument splitter. In other words, data
-- that the argument splitter sends to its /true/ sink goes to the /false/ sink of the result, and vice versa.
snot :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
snot = lift 1 "not" Combinator.sNot

-- | The '>&' combinator sends the /true/ sink output of its left operand to the input of its right operand for further
-- splitting. Both operands' /false/ sinks are connected to the /false/ sink of the combined splitter, but any input
-- value to reach the /true/ sink of the combined component data must be deemed true by both splitters.
(>&) :: forall m x b1 b2. MonadParallel m =>
        SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x (b1, b2)
(>&) = liftParallelPair ">&" Combinator.sAnd

-- | A '>|' combinator's input value can reach its /false/ sink only by going through both argument splitters' /false/
-- sinks.
(>|) :: forall m x b1 b2. MonadParallel m =>
        SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x (Either b1 b2)
(>|) = liftParallelPair ">&" Combinator.sOr

-- | Combinator '&&' is a pairwise logical conjunction of two splitters run in parallel on the same input.
(&&) :: forall m x b1 b2. MonadParallel m =>
        SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x (b1, b2)
(&&) = liftParallelPair "&&" Combinator.pAnd

-- | Combinator '||' is a pairwise logical disjunction of two splitters run in parallel on the same input.
(||) :: (MonadParallel m)
        => SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x (Either b1 b2)
(||) = liftParallelPair "||" Combinator.pOr

ifs :: forall c m x b. (MonadParallel m, Branching c m x ()) =>
       SplitterComponent m x b -> Component c -> Component c -> Component c
ifs = parallelRouterAndBranches "ifs" Combinator.ifs

wherever :: forall m x b. MonadParallel m =>
            TransducerComponent m x x -> SplitterComponent m x b -> TransducerComponent m x x
wherever = liftParallelPair "wherever" Combinator.wherever

unless :: forall m x b. MonadParallel m =>
          TransducerComponent m x x -> SplitterComponent m x b -> TransducerComponent m x x
unless = liftParallelPair "unless" Combinator.unless

select :: forall m x b. Monad m => SplitterComponent m x b -> TransducerComponent m x x
select = lift 1 "select" Combinator.select

-- | Converts a splitter into a parser.
parseRegions :: forall m x b. Monad m => SplitterComponent m x b -> ParserComponent m x b
parseRegions = lift 1 "parseRegions" Combinator.parseRegions

-- | Converts a boundary-marking splitter into a parser.
parseNestedRegions :: forall m x b. MonadParallel m =>
                      SplitterComponent m x (Boundary b) -> ParserComponent m x b
parseNestedRegions = lift 1 "parseNestedRegions" Combinator.parseNestedRegions

-- | The recursive combinator 'while' feeds the true sink of the argument splitter back to itself, modified by the
-- argument transducer. Data fed to the splitter's false sink is passed on unmodified.
while :: forall m x b. MonadParallel m =>
         TransducerComponent m x x -> SplitterComponent m x b -> TransducerComponent m x x
while t s = recursiveComponentTree "while" Combinator.while $ liftSequentialPair "pair" (,) t s

-- | The recursive combinator 'nestedIn' combines two splitters into a mutually recursive loop acting as a single
-- splitter.  The true sink of one of the argument splitters and false sink of the other become the true and false sinks
-- of the loop.  The other two sinks are bound to the other splitter's source.  The use of 'nestedIn' makes sense only
-- on hierarchically structured streams. If we gave it some input containing a flat sequence of values, and assuming
-- both component splitters are deterministic and stateless, an input value would either not loop at all or it would
-- loop forever.
nestedIn :: forall m x b. MonadParallel m =>
            SplitterComponent m x b -> SplitterComponent m x b -> SplitterComponent m x b
nestedIn s1 s2 = recursiveComponentTree "nestedIn" Combinator.nestedIn $ liftSequentialPair "pair" (,) s1 s2

-- | The 'foreach' combinator is similar to the combinator 'ifs' in that it combines a splitter and two transducers into
-- another transducer. However, in this case the transducers are re-instantiated for each consecutive portion of the
-- input as the splitter chunks it up. Each contiguous portion of the input that the splitter sends to one of its two
-- sinks gets transducered through the appropriate argument transducer as that transducer's whole input. As soon as the
-- contiguous portion is finished, the transducer gets terminated.
foreach :: forall m x b c. (MonadParallel m, Branching c m x ()) =>
           SplitterComponent m x b -> Component c -> Component c -> Component c
foreach = parallelRouterAndBranches "foreach" Combinator.foreach

-- | The 'having' combinator combines two pure splitters into a pure splitter. One splitter is used to chunk the input
-- into contiguous portions. Its /false/ sink is routed directly to the /false/ sink of the combined splitter. The
-- second splitter is instantiated and run on each portion of the input that goes to first splitter's /true/ sink. If
-- the second splitter sends any output at all to its /true/ sink, the whole input portion is passed on to the /true/
-- sink of the combined splitter, otherwise it goes to its /false/ sink.
having :: forall m x b1 b2. MonadParallel m =>
          SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x b1
having = liftParallelPair "having" Combinator.having

-- | The 'havingOnly' combinator is analogous to the 'having' combinator, but it succeeds and passes each chunk of the
-- input to its /true/ sink only if the second splitter sends no part of it to its /false/ sink.
havingOnly :: forall m x b1 b2. MonadParallel m =>
              SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x b1
havingOnly = liftParallelPair "havingOnly" Combinator.havingOnly

-- | The result of combinator 'first' behaves the same as the argument splitter up to and including the first portion of
-- the input which goes into the argument's /true/ sink. All input following the first true portion goes into the
-- /false/ sink.
first :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
first = lift 2 "first" Combinator.first

-- | The result of combinator 'uptoFirst' takes all input up to and including the first portion of the input which goes
-- into the argument's /true/ sink and feeds it to the result splitter's /true/ sink. All the rest of the input goes
-- into the /false/ sink. The only difference between 'first' and 'uptoFirst' combinators is in where they direct the
-- /false/ portion of the input preceding the first /true/ part.
uptoFirst :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
uptoFirst = lift 2 "uptoFirst" Combinator.uptoFirst

-- | The result of the combinator 'last' is a splitter which directs all input to its /false/ sink, up to the last
-- portion of the input which goes to its argument's /true/ sink. That portion of the input is the only one that goes to
-- the resulting component's /true/ sink.  The splitter returned by the combinator 'last' has to buffer the previous two
-- portions of its input, because it cannot know if a true portion of the input is the last one until it sees the end of
-- the input or another portion succeeding the previous one.
last :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
last = lift 2 "last" Combinator.last

-- | The result of the combinator 'lastAndAfter' is a splitter which directs all input to its /false/ sink, up to the
-- last portion of the input which goes to its argument's /true/ sink. That portion and the remainder of the input is
-- fed to the resulting component's /true/ sink. The difference between 'last' and 'lastAndAfter' combinators is where
-- they feed the /false/ portion of the input, if any, remaining after the last /true/ part.
lastAndAfter :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
lastAndAfter = lift 2 "lastAndAfter" Combinator.lastAndAfter

-- | The 'prefix' combinator feeds its /true/ sink only the prefix of the input that its argument feeds to its /true/
-- sink.  All the rest of the input is dumped into the /false/ sink of the result.
prefix :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
prefix = lift 2 "prefix" Combinator.prefix

-- | The 'suffix' combinator feeds its /true/ sink only the suffix of the input that its argument feeds to its /true/
-- sink.  All the rest of the input is dumped into the /false/ sink of the result.
suffix :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
suffix = lift 2 "suffix" Combinator.suffix

-- | The 'even' combinator takes every input section that its argument /splitter/ deems /true/, and feeds even ones into
-- its /true/ sink. The odd sections and parts of input that are /false/ according to its argument splitter are fed to
-- 'even' splitter's /false/ sink.
even :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x b
even = lift 2 "even" Combinator.even

-- | SplitterComponent 'startOf' issues an empty /true/ section at the beginning of every section considered /true/ by
-- its argument splitter, otherwise the entire input goes into its /false/ sink.
startOf :: forall m x b. Monad m => SplitterComponent m x b -> SplitterComponent m x (Maybe b)
startOf = lift 2 "startOf" Combinator.startOf

-- | SplitterComponent 'endOf' issues an empty /true/ section at the end of every section considered /true/ by its
-- argument splitter, otherwise the entire input goes into its /false/ sink.
endOf :: forall m x b. MonadParallel m => SplitterComponent m x b -> SplitterComponent m x (Maybe b)
endOf = lift 2 "endOf" Combinator.endOf

-- | Combinator 'followedBy' treats its argument 'SplitterComponent's as patterns components and returns a 'SplitterComponent' that
-- matches their concatenation. A section of input is considered /true/ by the result iff its prefix is considered
-- /true/ by argument /s1/ and the rest of the section is considered /true/ by /s2/. The splitter /s2/ is started anew
-- after every section split to /true/ sink by /s1/.
followedBy :: forall m x b1 b2. MonadParallel m =>
              SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x (b1, b2)
followedBy = liftParallelPair "followedBy" Combinator.followedBy

-- | Combinator '...' tracks the running balance of difference between the number of preceding starts of sections
-- considered /true/ according to its first argument and the ones according to its second argument. The combinator
-- passes to /true/ all input values for which the difference balance is positive. This combinator is typically used
-- with 'startOf' and 'endOf' in order to count entire input sections and ignore their lengths.
(...) :: forall m x b1 b2. MonadParallel m =>
         SplitterComponent m x b1 -> SplitterComponent m x b2 -> SplitterComponent m x b1
(...) = liftParallelPair "..." Combinator.between

xmlTokens :: Monad m => SplitterComponent m Char (Boundary Token)
xmlTokens = atomic "XML.tokens" 1 XML.tokens

xmlParseTokens :: Monad m => ParserComponent m Char Token
xmlParseTokens = atomic "XML.parseTokens" 1 XML.parseTokens

xmlElement :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlElement = atomic "XML.element" 1 XML.element

xmlElementContent :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlElementContent = atomic "XML.elementContent" 1 XML.elementContent

-- | Similiar to @('Control.Concurrent.SCC.Combinators.having' 'element')@, except it runs the argument splitter
-- only on each element's start tag, not on the entire element with its content.
xmlElementHavingTag :: forall m b. MonadParallel m =>
                       SplitterComponent m (Markup Token Char) b -> SplitterComponent m (Markup Token Char) b
xmlElementHavingTag = lift 2 "XML.elementHavingTag" XML.elementHavingTag

-- | Splits every attribute specification to /true/, everything else to /false/.
xmlAttribute :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlAttribute = atomic "XML.attribute" 1 XML.attribute

-- | Splits every element name, including the names of nested elements and names in end tags, to /true/, all the rest of
-- input to /false/.
xmlElementName :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlElementName = atomic "XML.elementName" 1 XML.elementName

-- | Splits every attribute name to /true/, all the rest of input to /false/.
xmlAttributeName :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlAttributeName = atomic "XML.attributeName" 1 XML.attributeName

-- | Splits every attribute value, excluding the quote delimiters, to /true/, all the rest of input to /false/.
xmlAttributeValue :: Monad m => SplitterComponent m (Markup Token Char) ()
xmlAttributeValue = atomic "XML.attributeValue" 1 XML.attributeValue

xmlHavingText :: forall m b1 b2. MonadParallel m =>
              SplitterComponent m (Markup Token Char) b1 -> SplitterComponent m Char b2 ->
              SplitterComponent m (Markup Token Char) b1
xmlHavingText = liftParallelPair "XML.havingText" XML.havingText

xmlHavingOnlyText :: forall m b1 b2. MonadParallel m =>
                     SplitterComponent m (Markup Token Char) b1 -> SplitterComponent m Char b2 ->
                     SplitterComponent m (Markup Token Char) b1
xmlHavingOnlyText = liftParallelPair "XML.havingOnlyText" XML.havingOnlyText
