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

-- | Module "XML" defines primitives and combinators for parsing and manipulating XML.

{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Control.Concurrent.SCC.XML (
-- * Types
Token (..),
-- * Parsing XML
tokens, parseTokens, expandEntity,
-- * Showing XML
escapeAttributeCharacter, escapeContentCharacter,
-- * Splitters
element, elementContent, elementName, attribute, attributeName, attributeValue,
-- * SplitterComponent combinators
elementHavingTag, havingText, havingOnlyText
)
where

import Prelude hiding (mapM)
import Control.Exception (assert)
import Control.Monad (join, liftM, when)
import Data.Char
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.List (find, stripPrefix)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Traversable (Traversable, mapM)
import Numeric (readDec, readHex)
import Debug.Trace (trace)

import Control.Monad.Coroutine
import Control.Monad.Parallel (MonadParallel(..))

import Control.Concurrent.SCC.Streams
import Control.Concurrent.SCC.Types
import Control.Concurrent.SCC.Combinators (groupMarks, splitterToMarker, parseNestedRegions,
                                           findsTrueIn, findsFalseIn, teeConsumers)


data Token = StartTag | EndTag | EmptyTag
           | ElementName | AttributeName | AttributeValue
           | EntityReferenceToken | EntityName
           | ProcessingInstruction | ProcessingInstructionText
           | Comment | CommentText
           | StartMarkedSectionCDATA | EndMarkedSection
           | ErrorToken String
             deriving (Eq, Show)

 
-- | Escapes a character for inclusion into an XML attribute value.
escapeAttributeCharacter :: Char -> String
escapeAttributeCharacter '"' = "&quot;"
escapeAttributeCharacter '\t' = "&#9;"
escapeAttributeCharacter '\n' = "&#10;"
escapeAttributeCharacter '\r' = "&#13;"
escapeAttributeCharacter x = escapeContentCharacter x

-- | Escapes a character for inclusion into the XML data content.
escapeContentCharacter :: Char -> String
escapeContentCharacter '<' = "&lt;"
escapeContentCharacter '&' = "&amp;"
escapeContentCharacter x = [x]

-- | Converts an XML entity name into the text value it represents: @expandEntity \"lt\" = \"<\"@.
expandEntity :: String -> String
expandEntity "lt" = "<"
expandEntity "gt" = ">"
expandEntity "quot" = "\""
expandEntity "apos" = "'"
expandEntity "amp" = "&"
expandEntity ('#' : 'x' : codePoint) = [chr (fst $ head $ readHex codePoint)]
expandEntity ('#' : codePoint) = [chr (fst $ head $ readDec codePoint)]

isNameStart x = isLetter x || x == '_'
isNameChar x = isAlphaNum x || x == '_' || x == '-'

-- | The 'tokens' splitter distinguishes XML markup from data content. It is used by 'parseTokens'.
tokens :: Monad m => Splitter m Char (Boundary Token)
tokens = Splitter $
         \source true false edge->
         let getContent = getWith content source
             content '<' = getWith (\x-> tag x >> getWith content source) source
             content '&' = entity >> getWith content source
             content x = put false x
                         >> getContent
             tag '?' = do put edge (Start ProcessingInstruction)
                          putList "<?" true
                          put edge (Start ProcessingInstructionText)
                          processingInstruction
             tag '!' = dispatchOnString source
                          (\other-> put edge (Point (errorBadDeclarationType other)))
                          [("--",
                            \match-> do put edge (Start Comment)
                                        putList match true
                                        put edge (Start CommentText)
                                        comment),
                           ("[CDATA[",
                            \match-> do put edge (Start StartMarkedSectionCDATA)
                                        putList match true
                                        put edge (End StartMarkedSectionCDATA)
                                        markedSection)]
             tag '/' = {-# SCC "EndTag" #-}
                       do put edge (Start EndTag)
                          put true '<'
                          put true '/'
                          next errorInputEndInEndTag
                               (\x-> name ElementName x
                                        >>= maybe
                                               (put edge (Point errorInputEndInEndTag))
                                               (\x-> do put true x
                                                        when (x /= '>') (put edge (Point (errorBadEndTag x)))))
                          put edge (End EndTag)
             tag x | isNameStart x = {-# SCC "StartTag" #-}
                                     put edge (Start StartTag)
                                     >> put true '<'
                                     >> name ElementName x
                                     >>= maybe
                                            (put edge (Point errorInputEndInStartTag))
                                            (\y-> attributes y
                                                  >>= maybe
                                                         (put edge (Point errorInputEndInStartTag))
                                                         startTagEnd)
                                     >> put edge (End StartTag)
             tag x = put edge (Point errorUnescapedContentLT)
                     >> put false '<'
                     >> put false x
             startTagEnd '/' = put true '/'
                               >> put edge (Point EmptyTag)
                               >> next errorInputEndInStartTag
                                     (\x-> put true x >> when (x /= '>') (put edge (Point (errorBadStartTag x))))
             startTagEnd '>' = put true '>'
             startTagEnd x = put true x
                             >> put edge (Point (errorBadStartTag x))
             attributes x | isSpace x = put true x >> get source >>= mapJoinM attributes
             attributes x | isNameStart x
                = name AttributeName x
                  >>= mapJoinM
                         (\y-> do when (y /= '=') (put edge (Point (errorBadAttribute y)))
                                  q <- if y == '"' || y == '\''
                                       then return y
                                       else put true y >> get source
                                            >>= maybe
                                                   (put edge (Point errorInputEndInAttributeValue)
                                                    >> return '"')
                                                   return
                                  when (q /= '"' && q /= '\'') (put edge (Point (errorBadQuoteCharacter q)))
                                  put true q
                                  put edge (Start AttributeValue)
                                  get source
                                         >>= maybe
                                                (put edge (Point errorInputEndInAttributeValue)
                                                 >> put edge (End AttributeValue))
                                                (attributeValue q)
                                  get source >>= mapJoinM attributes)
             attributes x = return (Just x)
             attributeValue q x | q == x = do put edge (End AttributeValue)
                                              put true x
             attributeValue q '<' = do put edge (Start errorUnescapedAttributeLT)
                                       put true '<'
                                       put edge (End errorUnescapedAttributeLT)
                                       next errorInputEndInAttributeValue (attributeValue q)
             attributeValue q '&' = entity >> next errorInputEndInAttributeValue (attributeValue q)
             attributeValue q x = put true x >> next errorInputEndInAttributeValue (attributeValue q)
             processingInstruction = {-# SCC "PI" #-}
                                     dispatchOnString source
                                        (\other-> if null other
                                                  then put edge (Point errorInputEndInProcessingInstruction)
                                                  else putList other true >> processingInstruction)
                                        [("?>",
                                          \match-> do put edge (End ProcessingInstructionText)
                                                      putList match true
                                                      put edge (End ProcessingInstruction)
                                                      getContent)]
             comment = {-# SCC "comment" #-}
                       dispatchOnString source
                          (\other-> if null other
                                    then put edge (Point errorInputEndInComment)
                                    else putList other true >> comment)
                          [("-->",
                            \match-> do put edge (End CommentText)
                                        putList match true
                                        put edge (End Comment)
                                        getContent)]
             markedSection = {-# SCC "<![CDATA[" #-}
                             dispatchOnString source
                                (\other-> if null other
                                          then put edge (Point errorInputEndInMarkedSection)
                                          else putList other true >> markedSection)
                                [("]]>",
                                  \match-> do put edge (Start EndMarkedSection)
                                              putList match true
                                              put edge (End EndMarkedSection)
                                              getContent)]
             entity = put edge (Start EntityReferenceToken)
                      >> put true '&'
                      >> next errorInputEndInEntityReference
                            (\x-> name EntityName x
                                  >>= maybe 
                                         (put edge (Point errorInputEndInEntityReference))
                                         (\x-> do when (x /= ';') (put edge (Point (errorBadEntityReference x)))
                                                  put true x))
                      >> put edge (End EntityReferenceToken)
             name token x | isNameStart x = {-# SCC "name" #-}
                                            put edge (Start token)
                                            >> put true x
                                            >> get source
                                            >>= maybe
                                                   (put edge (End token) >> return Nothing)
                                                   (nameTail token)
             name _ x = return (Just x)
             nameTail token x = if isNameChar x || x == ':'
                                then put true x
                                     >> get source
                                     >>= maybe
                                            (put edge (End token) >> return Nothing)
                                            (nameTail token)
                                else put edge (End token) >> return (Just x)
             next error f = get source
                            >>= maybe (put edge (Point error)) f
         in getContent

errorInputEndInComment = ErrorToken "Unterminated comment"
errorInputEndInMarkedSection = ErrorToken "Unterminated marked section"
errorInputEndInStartTag = ErrorToken "Missing '>' at the end of start tag."
errorInputEndInEndTag = ErrorToken "End of input in end tag"
errorInputEndInAttributeValue = ErrorToken "Truncated input after attribute name"
errorInputEndInEntityReference = ErrorToken "End of input in entity reference"
errorInputEndInProcessingInstruction = ErrorToken "Unterminated processing instruction"
errorBadQuoteCharacter q = ErrorToken ("Invalid quote character " ++ show q)
errorBadStartTag x = ErrorToken ("Invalid character " ++ show x ++ " in start tag")
errorBadEndTag x = ErrorToken ("Invalid character " ++ show x ++ " in end tag")
errorBadAttribute x = ErrorToken ("Invalid character " ++ show x ++ " following attribute name")
errorBadAttributeValue x = ErrorToken ("Invalid character " ++ show x ++ " in attribute value.")
errorBadEntityReference x = ErrorToken ("Invalid character " ++ show x ++ " ends entity name.")
errorBadDeclarationType other = ErrorToken ("Expecting <![CDATA[ or <!--, received " ++ show ("<![" ++ other))
errorUnescapedContentLT = ErrorToken "Unescaped character '<' in content"
errorUnescapedAttributeLT = ErrorToken "Invalid character '<' in attribute value."

-- | The XML token parser. This parser converts plain text to parsed text, which is a precondition for using the
-- remaining XML components.
parseTokens :: Monad m => Parser m Char Token
parseTokens = parseNestedRegions tokens

dispatchOnString :: forall m a d r. (Monad m, AncestorFunctor a d) =>
                    Source m a Char -> (String -> Coroutine d m r) -> [(String, String -> Coroutine d m r)]
                 -> Coroutine d m r
dispatchOnString source failure fullCases = dispatch fullCases id
   where dispatch cases consumed
            = case find (null . fst) cases
              of Just ("", rhs) -> rhs (consumed "")
                 Nothing -> get source
                            >>= maybe
                                   (failure (consumed ""))
                                   (\x-> case mapMaybe (startingWith x) cases
                                         of [] -> failure (consumed [x])
                                            subcases -> dispatch (subcases ++ fullCases) (consumed . (x :)))
         startingWith x (y:rest, rhs) | x == y = Just (rest, rhs)
                                      | otherwise = Nothing

getElementName :: forall m a d. (Monad m, AncestorFunctor a d) =>
                  Source m a (Markup Token Char) -> ([Markup Token Char] -> [Markup Token Char])
               -> Coroutine d m ([Markup Token Char], Maybe String)
getElementName source f = get source
                          >>= maybe
                                 (return (f [], Nothing))
                                 (\x-> case x
                                       of Markup (Start ElementName) -> getRestOfRegion ElementName source (f . (x:)) id
                                          Markup (Point ErrorToken{}) -> getElementName source (f . (x:))
                                          Content{} -> getElementName source (f . (x:))
                                          _ -> error ("Expected an ElementName, received " ++ show x))

getRestOfRegion :: forall m a d. (Monad m, AncestorFunctor a d) =>
                   Token -> Source m a (Markup Token Char)
                -> ([Markup Token Char] -> [Markup Token Char]) -> (String -> String)
                -> Coroutine d m ([Markup Token Char], Maybe String)
getRestOfRegion token source f g = get source
                                   >>= maybe
                                          (return (f [], Nothing))
                                          (\x-> case x
                                                of Markup (End token) -> return (f [x], Just (g ""))
                                                   Content y -> getRestOfRegion token source (f . (x:)) (g . (y:))
                                                   _ -> error ("Expected rest of " ++ show token
                                                               ++ ", received " ++ show x))

pourRestOfRegion :: forall m a1 a2 a3 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d) =>
                    Token -> Source m a1 (Markup Token Char)
                          -> Sink m a2 (Markup Token Char) -> Sink m a3 (Markup Token Char)
                 -> Coroutine d m Bool
pourRestOfRegion token source sink endSink
   = get source
     >>= maybe
            (return False)
            (\x-> case x
                  of Markup (End token') | token == token' -> put endSink x
                                                              >> return True
                     Content y -> put sink x
                                  >> pourRestOfRegion token source sink endSink
                     _ -> error ("Expected rest of " ++ show token ++ ", received " ++ show x))

pourRestOfTag :: forall m a1 a2 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d) =>
                 Source m a1 (Markup Token Char) -> Sink m a2 (Markup Token Char) -> Coroutine d m Bool
pourRestOfTag source sink = get source
                            >>= maybe
                                   (return True)
                                   (\x-> put sink x
                                         >> case x of Markup (End StartTag) -> return True
                                                      Markup (End EndTag) -> return True
                                                      Markup (Point EmptyTag) -> pourRestOfTag source sink
                                                                                 >> return False
                                                      _ -> pourRestOfTag source sink)

findEndTag :: forall m a1 a2 a3 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d, AncestorFunctor a3 d) =>
              Source m a1 (Markup Token Char) -> Sink m a2 (Markup Token Char) -> Sink m a3 (Markup Token Char)
                                              -> String
           -> Coroutine d m ()
findEndTag source sink endSink name = find where
   find = getWith consumeOne source
   consumeOne x@(Markup (Start EndTag)) = do (tokens, mn) <- getElementName source (x :)
                                             maybe
                                                (return ())
                                                (\name'-> if name == name'
                                                          then do putList tokens endSink
                                                                  pourRestOfTag source endSink
                                                                  return ()
                                                          else do putList tokens sink
                                                                  pourRestOfTag source sink
                                                                  find)
                                                mn
   consumeOne x@(Markup (Start StartTag)) = do (tokens, mn) <- getElementName source (x :)
                                               maybe
                                                  (return ())
                                                  (\name'-> putList tokens sink
                                                            >> if name == name'
                                                               then pourRestOfTag source sink
                                                                    >>= flip when (findEndTag source sink sink name)
                                                                    >> find
                                                               else pourRestOfTag source sink
                                                                    >> find)
                                                  mn
   consumeOne x = put sink x >> find

findStartTag :: forall m a1 a2 d. (Monad m, AncestorFunctor a1 d, AncestorFunctor a2 d) =>
                Source m a1 (Markup Token Char) -> Sink m a2 (Markup Token Char)
             -> Coroutine d m (Maybe (Markup Token Char))
findStartTag source sink = get source
                           >>= maybe
                                  (return Nothing)
                                  (\x-> case x of Markup (Start StartTag) -> return $ Just x
                                                  _ -> put sink x
                                                       >> findStartTag source sink)

-- | Splits all top-level elements with all their content to /true/, all other input to /false/.
element :: Monad m => Splitter m (Markup Token Char) ()
element = Splitter $
          \source true false edge->
          let split0 = findStartTag source false
                       >>= maybe (return ())
                              (\x-> do put edge ()
                                       put true x
                                       (tokens, mn) <- getElementName source id
                                       maybe
                                          (putList tokens true)
                                          (\name-> putList tokens true
                                                   >> pourRestOfTag source true
                                                   >>= cond
                                                          (split1 name)
                                                          split0)
                                             mn)
              split1 name = findEndTag source true true name
                            >> split0
          in split0

-- | Splits the content of all top-level elements to /true/, their tags and intervening input to /false/.
elementContent :: Monad m => Splitter m (Markup Token Char) ()
elementContent = Splitter $
                 \source true false edge->
                 let split0 = findStartTag source false
                              >>= maybe (return ())
                                     (\x-> do put false x
                                              (tokens, mn) <- getElementName source id
                                              maybe
                                                 (putList tokens false)
                                                 (\name-> putList tokens false
                                                          >> pourRestOfTag source false
                                                          >>= cond
                                                                 (put edge ()
                                                                  >> split1 name)
                                                                 split0)
                                                 mn)
                     split1 name = findEndTag source true false name
                                   >> split0
                 in split0

-- | Similiar to @('Control.Concurrent.SCC.Combinators.having' 'element')@, except it runs the argument splitter
-- only on each element's start tag, not on the entire element with its content.
elementHavingTag :: forall m b. MonadParallel m =>
                    Splitter m (Markup Token Char) b -> Splitter m (Markup Token Char) b
elementHavingTag test =
   isolateSplitter $ \ source true false edge ->
      let split0 = findStartTag source false
                   >>= maybe (return ())
                          (\x-> do (tokens, mn) <- getElementName source (x :)
                                   maybe
                                      (return ())
                                      (\name-> do (hasContent, rest) <- pipe
                                                                           (pourRestOfTag source)
                                                                           getList
                                                  let tag = tokens ++ rest
                                                  ((), found) <- pipe (putList tag) (findsTrueIn test)
                                                  case found of Just mb -> maybe (return ()) (put edge) mb
                                                                           >> putList tag true
                                                                           >> split1 hasContent true name
                                                                Nothing -> putList tag false
                                                                           >> split1 hasContent false name)
                                      mn)
          split1 hasContent sink name = when hasContent (findEndTag source sink sink name)
                                        >> split0
   in split0

-- | Splits every attribute specification to /true/, everything else to /false/.
attribute :: Monad m => Splitter m (Markup Token Char) ()
attribute = Splitter $
            \source true false edge->
            let split0 = getWith
                            (\x-> case x
                                  of Markup (Start AttributeName) -> do put edge ()
                                                                        put true x
                                                                        pourRestOfRegion AttributeName source true true
                                                                                            >>= flip when split1
                                     _ -> put false x >> split0)
                            source
                split1 = getWith
                            (\x-> case x
                                  of Markup (Start AttributeValue)
                                        -> put true x
                                           >> pourRestOfRegion AttributeValue source true true
                                           >>= flip when split0
                                     _ -> put true x >> split1)
                            source
            in split0

-- | Splits every element name, including the names of nested elements and names in end tags, to /true/, all the rest of
-- input to /false/.
elementName :: Monad m => Splitter m (Markup Token Char) ()
elementName = Splitter (splitSimpleRegions ElementName)

-- | Splits every attribute name to /true/, all the rest of input to /false/.
attributeName :: Monad m => Splitter m (Markup Token Char) ()
attributeName = Splitter  (splitSimpleRegions AttributeName)

-- | Splits every attribute value, excluding the quote delimiters, to /true/, all the rest of input to /false/.
attributeValue :: Monad m => Splitter m (Markup Token Char) ()
attributeValue = Splitter (splitSimpleRegions AttributeValue)

splitSimpleRegions token source true false edge = split
   where split = getWith consumeOne source
         consumeOne x@(Markup (Start token')) | token == token' = put false x
                                                                  >> put edge ()
                                                                  >> pourRestOfRegion token source true false
                                                                  >>= flip when split
         consumeOne x = put false x >> split

-- | Behaves like 'Control.Concurrent.SCC.Combinators.having', but the right-hand splitter works on plain instead of
-- marked-up text. This allows regular 'Char' splitters to be applied to parsed XML.
havingText :: forall m b1 b2. MonadParallel m =>
              Bool -> Splitter m (Markup Token Char) b1 -> Splitter m Char b2 -> Splitter m (Markup Token Char) b1
havingText parallel chunker tester = isolateSplitter havingText' where
   havingText' source true false edge =
      let test Nothing chunk = pour chunk false
          test (Just mb) chunk = teeConsumers False getList (findsTrueIn tester . mapMaybeSource justContent) chunk
                                 >>= \(chunk, found)->
                                     if isJust found
                                     then maybe (return ()) (put edge) mb
                                          >> putList chunk true
                                     else putList chunk false
      in liftM fst $
         pipePS parallel
            (transduce (splitterToMarker chunker) source)
            (flip groupMarks test)

-- | Behaves like 'Control.Concurrent.SCC.Combinators.havingOnly', but the right-hand splitter works on plain instead of
-- marked-up text. This allows regular 'Char' splitters to be applied to parsed XML.
havingOnlyText :: forall m b1 b2. MonadParallel m =>
                  Bool -> Splitter m (Markup Token Char) b1 -> Splitter m Char b2 -> Splitter m (Markup Token Char) b1
havingOnlyText parallel chunker tester = isolateSplitter havingOnlyText' where
   havingOnlyText' source true false edge =
      let test Nothing chunk = pour chunk false
          test (Just mb) chunk = teeConsumers False getList (findsFalseIn tester . mapMaybeSource justContent) chunk
                                 >>= \(chunk, found)->
                                     if found
                                     then putList chunk false
                                     else maybe (return ()) (put edge) mb
                                          >> putList chunk true
      in liftM fst $
         pipePS parallel
            (transduce (splitterToMarker chunker) source)
            (flip groupMarks test)

justContent (Content x) = Just x
justContent _ = Nothing

mapJoinM :: (Monad m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
mapJoinM f ta = mapM f ta >>= return . join

