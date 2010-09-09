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

-- | Module "Primitives" defines primitive components of 'Producer', 'Consumer', 'Transducer' and 'Splitter' types,
-- defined in the "Types" module.

{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

module Control.Concurrent.SCC.Primitives
   (
    -- * Tag types
    OccurenceTag,
    -- * List producers and consumers
    fromList, toList,
    -- * I/O producers and consumers
    fromFile, fromHandle, fromStdIn,
    appendFile, toFile, toHandle, toStdOut,
    -- * Generic consumers
    suppress, erroneous,
    -- * Generic transducers
    parse, unparse, parseSubstring,
    -- * Generic splitters
    everything, nothing, marked, markedContent, markedWith, contentMarkedWith, one, substring,
    -- * List transducers
    -- | The following laws hold:
    --
    --    * 'group' '>>>' 'concatenate' == 'id'
    --
    --    * 'concatenate' == 'concatSeparate' []
    group, concatenate, concatSeparate,
    -- * Character stream components
    lowercase, uppercase, whitespace, letters, digits, line, nonEmptyLine,
    -- * Oddballs
    count, toString
)
where

import Prelude hiding (appendFile)

import Control.Monad.Coroutine
import Control.Concurrent.SCC.Streams
import Control.Concurrent.SCC.Types

import Control.Exception (assert)

import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad as Monad
import Data.Char (isAlpha, isDigit, isPrint, isSpace, toLower, toUpper)
import Data.List (delete, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (><), ViewL (EmptyL, (:<)))
import Debug.Trace (trace)
import System.IO (Handle, IOMode (ReadMode, WriteMode, AppendMode), openFile, hClose,
                  hGetChar, hPutChar, hFlush, hIsEOF, hClose, putChar, isEOF, stdout)

-- | Consumer 'toList' copies the given source into a list.
toList :: forall m x. Monad m => Consumer m x [x]
toList = Consumer getList

-- | 'fromList' produces the contents of the given list argument.
fromList :: forall m x. Monad m => [x] -> Producer m x ()
fromList l = Producer (putList l)

-- | Consumer 'toStdOut' copies the given source into the standard output.
toStdOut :: Consumer IO Char ()
toStdOut = Consumer (mapMStream_ (\x-> lift (putChar x)))

-- | Producer 'fromStdIn' feeds the given sink from the standard input.
fromStdIn :: Producer IO Char ()
fromStdIn = Producer (unmapMStream_ (lift isEOF >>= cond (return Nothing) (lift (liftM Just getChar))))

-- | Producer 'fromFile' opens the named file and feeds the given sink from its contents.
fromFile :: String -> Producer IO Char ()
fromFile path = Producer $ \sink-> do handle <- lift (openFile path ReadMode)
                                      produce (fromHandle handle True) sink

-- | Producer 'fromHandle' feeds the given sink from the open file /handle/. The argument /doClose/ determines
-- | if /handle/ should be closed when the handle is consumed or the sink closed.
fromHandle :: Handle -> Bool -> Producer IO Char ()
fromHandle handle doClose = Producer (\sink-> unmapMStream_ (lift hGetCharMaybe) sink
                                              >> when doClose (lift $ hClose handle))
   where hGetCharMaybe = hIsEOF handle >>= cond (return Nothing) (liftM Just $ hGetChar handle)
            

-- | Consumer 'toFile' opens the named file and copies the given source into it.
toFile :: String -> Consumer IO Char ()
toFile path = Consumer $ \source-> do handle <- lift (openFile path WriteMode)
                                      consume (toHandle handle True) source

-- | Consumer 'appendFile' opens the name file and appends the given source to it.
appendFile :: String -> Consumer IO Char ()
appendFile path = Consumer $ \source-> do handle <- lift (openFile path AppendMode)
                                          consume (toHandle handle True) source

-- | Consumer 'toHandle' copies the given source into the open file /handle/. The argument /doClose/ determines
-- | if /handle/ should be closed once the entire source is consumed and copied.
toHandle :: Handle -> Bool -> Consumer IO Char ()
toHandle handle doClose = Consumer (\source-> mapMStream_ (lift . hPutChar handle) source
                                              >> when doClose (lift $ hClose handle))

-- | Transducer 'unparse' removes all markup from its input and passes the content through.
unparse :: forall m x y. Monad m => Transducer m (Markup y x) x
unparse = statelessTransducer removeTag
   where removeTag (Content x) = [x]
         removeTag _ = []

-- | Transducer 'parse' prepares input content for subsequent parsing.
parse :: forall m x y. Monad m => Transducer m x (Markup y x)
parse = oneToOneTransducer Content

-- | The 'suppress' consumer suppresses all input it receives. It is equivalent to 'substitute' []
suppress :: forall m x y. Monad m => Consumer m x ()
suppress = Consumer (mapMStream_ (const $ return ()))

-- | The 'erroneous' consumer reports an error if any input reaches it.
erroneous :: forall m x. Monad m => String -> Consumer m x ()
erroneous message = Consumer (getWith (const (error message)))

-- | The 'lowercase' transforms all uppercase letters in the input to lowercase, leaving the rest unchanged.
lowercase :: forall m. Monad m => Transducer m Char Char
lowercase = oneToOneTransducer toLower

-- | The 'uppercase' transforms all lowercase letters in the input to uppercase, leaving the rest unchanged.
uppercase :: forall m. Monad m => Transducer m Char Char
uppercase = oneToOneTransducer toUpper

-- | The 'count' transducer counts all its input values and outputs the final tally.
count :: forall m x. Monad m => Transducer m x Integer
count = Transducer (\source sink-> foldStream (\count _-> succ count) 0 source >>= put sink)

-- | Converts each input value @x@ to @show x@.
toString :: forall m x. (Monad m, Show x) => Transducer m x String
toString = oneToOneTransducer show

-- | Transducer 'group' collects all its input values into a single list.
group :: forall m x. Monad m => Transducer m x [x]
group = Transducer (\source sink-> foldStream (|>) Seq.empty source >>= put sink . Foldable.toList)

-- | Transducer 'concatenate' flattens the input stream of lists of values into the output stream of values.
concatenate :: forall m x. Monad m => Transducer m [x] x
concatenate = statelessTransducer id

-- | Same as 'concatenate' except it inserts the given separator list between every two input lists.
concatSeparate :: forall m x. Monad m => [x] -> Transducer m [x] x
concatSeparate separator = statefulTransducer (\seen list-> (True, if seen then separator ++ list else list))
                                              False

-- | Splitter 'whitespace' feeds all white-space characters into its /true/ sink, all others into /false/.
whitespace :: forall m. Monad m => Splitter m Char ()
whitespace = statelessSplitter isSpace

-- | Splitter 'letters' feeds all alphabetical characters into its /true/ sink, all other characters into
-- | /false/.
letters :: forall m. Monad m => Splitter m Char ()
letters = statelessSplitter isAlpha

-- | Splitter 'digits' feeds all digits into its /true/ sink, all other characters into /false/.
digits :: forall m. Monad m => Splitter m Char ()
digits = statelessSplitter isDigit

-- | Splitter 'nonEmptyLine' feeds line-ends into its /false/ sink, and all other characters into /true/.
nonEmptyLine :: forall m. Monad m => Splitter m Char ()
nonEmptyLine = statelessSplitter (\ch-> ch /= '\n' && ch /= '\r')

-- | The sectioning splitter 'line' feeds line-ends into its /false/ sink, and line contents into /true/. A single
-- line-end can be formed by any of the character sequences \"\\n\", \"\\r\", \"\\r\\n\", or \"\\n\\r\".
line :: forall m. Monad m => Splitter m Char ()
line = Splitter $
       \source true false boundaries-> let split Nothing x = put boundaries () >> handle x
                                           split (Just '\n') x@'\r' = put false x >> return Nothing
                                           split (Just '\r') x@'\n' = put false x >> return Nothing
                                           split (Just '\n') x = split Nothing x
                                           split (Just '\r') x = split Nothing x
                                           split (Just _) x = handle x
                                           handle x = (if x == '\n' || x == '\r'
                                                       then put false x
                                                       else put true x)
                                                      >> return (Just x)
                                       in foldMStream_ split Nothing source

-- | Splitter 'everything' feeds its entire input into its /true/ sink.
everything :: forall m x. Monad m => Splitter m x ()
everything = Splitter (\source true false edge-> put edge () >> pour source true)

-- | Splitter 'nothing' feeds its entire input into its /false/ sink.
nothing :: forall m x. Monad m => Splitter m x ()
nothing = Splitter (\source true false edge-> pour source false)

-- | Splitter 'one' feeds all input values to its /true/ sink, treating every value as a separate section.
one :: forall m x. Monad m => Splitter m x ()
one = Splitter (\source true false edge-> mapMStream_ (\x-> put edge () >> put true x) source)

-- | Splitter 'marked' passes all marked-up input sections to its /true/ sink, and all unmarked input to its
-- /false/ sink.
marked :: forall m x y. (Monad m, Eq y) => Splitter m (Markup y x) ()
marked = markedWith (const True)

-- | Splitter 'markedContent' passes the content of all marked-up input sections to its /true/ sink, while the
-- outermost tags and all unmarked input go to its /false/ sink.
markedContent :: forall m x y. (Monad m, Eq y) => Splitter m (Markup y x) ()
markedContent = contentMarkedWith (const True)

-- | Splitter 'markedWith' passes input sections marked-up with the appropriate tag to its /true/ sink, and the
-- rest of the input to its /false/ sink. The argument /select/ determines if the tag is appropriate.
markedWith :: forall m x y. (Monad m, Eq y) => (y -> Bool) -> Splitter m (Markup y x) ()
markedWith select = statefulSplitter transition ([], False)
   where transition s@([], _)     Content{} = (s, False)
         transition s@(_, truth)  Content{} = (s, truth)
         transition s@([], _)     (Markup (Point y)) = (s, select y)
         transition s@(_, truth)  (Markup (Point y)) = (s, truth)
         transition ([], _)       (Markup (Start y)) = (([y], select y), select y)
         transition (open, truth) (Markup (Start y)) = ((y:open, truth), truth)
         transition (open, truth) (Markup (End y))   = assert (elem y open) ((delete y open, truth), truth)

-- | Splitter 'contentMarkedWith' passes the content of input sections marked-up with the appropriate tag to
-- its /true/ sink, and the rest of the input to its /false/ sink. The argument /select/ determines if the tag is
-- appropriate.
contentMarkedWith :: forall m x y. (Monad m, Eq y) => (y -> Bool) -> Splitter m (Markup y x) ()
contentMarkedWith select = statefulSplitter transition ([], False)
   where transition s@(_, truth)  Content{} = (s, truth)
         transition s@(_, truth)  (Markup Point{}) = (s, truth)
         transition ([], _)       (Markup (Start y)) = (([y], select y), False)
         transition (open, truth) (Markup (Start y)) = ((y:open, truth), truth)
         transition (open, truth) (Markup (End y))   = assert (elem y open) (let open' = delete y open
                                                                                 truth' = not (null open') && truth
                                                                             in ((open', truth'), truth'))

-- | Used by 'parseSubstring' to distinguish between overlapping substrings.
data OccurenceTag = Occurence Int deriving (Eq, Show)

instance Enum OccurenceTag where
   succ (Occurence n) = Occurence (succ n)
   pred (Occurence n) = Occurence (pred n)
   toEnum = Occurence
   fromEnum (Occurence n) = n

-- | Performs the same task as the 'substring' splitter, but instead of splitting it outputs the input as @'Markup' x
-- 'OccurenceTag'@ in order to distinguish overlapping strings.
parseSubstring :: forall m x y. (Monad m, Eq x) => [x] -> Parser m x OccurenceTag
parseSubstring [] = Transducer $ \ source sink ->
                    put sink marker >> mapMStream_ (\x-> put sink (Content x) >> put sink marker) source
   where marker = Markup (Point (toEnum 1))
parseSubstring list
   = Transducer $
     \ source sink ->
        let getNext id rest q = get source
                                >>= maybe
                                       (flush q)
                                       (advance id rest q)
            advance id rest@(head:tail) q x = let q' = q |> Content x
                                                  view@(qh@Content{} :< qt) = Seq.viewl q'
                                                  id' = succ id
                                              in if x == head
                                                 then if null tail
                                                      then put sink (Markup (Start (toEnum id')))
                                                           >> put sink qh
                                                           >> (fallback id' (qt |> Markup (End (toEnum id'))))
                                                      else getNext id tail q'
                                                 else fallback id q'
            fallback id q = case Seq.viewl q
                            of EmptyL -> getNext id list q
                               head@(Markup (End id')) :< tail -> put sink head
                                                                  >> fallback
                                                                        (if id == fromEnum id' then 0 else id)
                                                                        tail
                               view@(head@Content{} :< tail) -> case stripPrefix (remainingContent q) list
                                                                of Just rest -> getNext id rest q
                                                                   Nothing -> put sink head
                                                                              >> fallback id tail
            flush q = putQueue q sink
            remainingContent :: Seq (Markup OccurenceTag x) -> [x]
            remainingContent q = extractContent (Seq.viewl q)
            extractContent :: Foldable.Foldable f => f (Markup b x) -> [x]
            extractContent = Foldable.concatMap (\e-> case e of {Content x -> [x]; _ -> []})
        in getNext 0 list Seq.empty

-- | Splitter 'substring' feeds to its /true/ sink all input parts that match the contents of the given list
-- argument. If two overlapping parts of the input both match the argument, both are sent to /true/ and each is preceded
-- by an edge.
substring :: forall m x. (Monad m, Eq x) => [x] -> Splitter m x ()
substring [] = Splitter $ \ source true false edge -> split one source false true edge >> put edge ()
substring list
   = Splitter $
     \ source true false edge ->
        let getNext rest qt qf = get source
                                 >>= maybe
                                        (putList (Foldable.toList (Seq.viewl qt)) true
                                         >> putList (Foldable.toList (Seq.viewl qf)) false)
                                        (advance rest qt qf)
            advance rest@(head:tail) qt qf x = let qf' = qf |> x
                                                   view@(qqh :< qqt) = Seq.viewl (qt >< qf')
                                               in if x == head
                                                  then if null tail
                                                       then put edge ()
                                                            >> put true qqh
                                                            >> fallback qqt Seq.empty
                                                      else getNext tail qt qf'
                                                 else fallback qt qf'
            fallback qt qf = case Seq.viewl (qt >< qf)
                             of EmptyL -> getNext list Seq.empty Seq.empty
                                view@(head :< tail) -> case stripPrefix (Foldable.toList view) list
                                                       of Just rest -> getNext rest qt qf
                                                          Nothing -> if Seq.null qt
                                                                     then put false head
                                                                          >> fallback Seq.empty tail
                                                                     else put true head
                                                                          >> fallback (Seq.drop 1 qt) qf
        in getNext list Seq.empty Seq.empty
