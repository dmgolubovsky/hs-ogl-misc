{-# Language CPP, MultiParamTypeClasses, FunctionalDependencies, 
                  FlexibleInstances, TypeSynonymInstances, PostfixOperators #-}
------------------------------------------------------------------
-- |
-- Module      :  Text.Yaml.EnumTok
-- Copyright   :  (c) portions by Oren Ben-Kiki, 
--                    portions by Dmitry Golubovsky, 2010
-- License     :  LGPL (by inheritance)
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Enumerator/Iteratee compatible YAML tokenizer.
------------------------------------------------------------------

-- This module heavily borrows from the YamlReference package,
-- but its goal is to provide an iteratee-compatible tokenizer.
-- Such tokenizer, given a chunk of input, would yield some tokens
-- and a continuation that receives the next portion of input.

module Text.Yaml.EnumTok (
  Code (..)
 ,Token (..)
 ,Reply (..)
 ,Result (..)
 ,State (..)
 ,Parser (..)
 ,stepTok
 ,initTok
 ,loopTok
) where

import Prelude hiding ((/), (*), (+), (-), (^))
import Data.Char
import qualified Prelude
import Text.Yaml.Types
import qualified Data.DList as D
import qualified Data.Map as Map

-- * Generic operators
--
-- ** Numeric operators
--
-- We rename the four numerical operators @+@ @-@ @*@ @\/@ to start with @.@
-- (@.+@, @.-@, @.*@, @.\/@). This allows us to use the originals for BNF
-- notation (we also hijack the @^@ operator). This is not a generally
-- recommended practice. It is justified in this case since we have very little
-- arithmetic operations, and a lot of BNF rules which this makes extremely
-- readable.

infixl 6 .+
-- | \".+\" is the numeric addition (we use \"+\" for postfix \"one or more\").
(.+) = (Prelude.+)

infixl 6 .-
-- | \".-\" is the numeric subtraction (we use \"-\" for infix \"and not\").
(.-) = (Prelude.-)

infixl 7 .*
-- | \".*\" is the numeric multiplication (we use \"*\" for postfix \"zero or
-- more\").
(.*) = (Prelude.*)

infixl 7 ./
-- | \"./\" is the numeric division (we use \"/\" for infix \"or\").
(./) = (Prelude./)

-- ** Record field access
--
-- We also define @|>@ for record access for increased readability.

infixl 9 |>
-- | @record |> field@ is the same as @field record@,  but is more readable.
(|>) :: record -> (record -> value) -> value
record |> field = field record

-- | @show token@ converts a 'Token' to two YEAST lines: a comment with the
-- position numbers and the actual token line.
instance Show Token where
  show token =   "C: " ++ (show $ token|>tCharOffset)
            ++ ", L: " ++ (show $ token|>tLine)
            ++ ", c: " ++ (show $ token|>tLineChar) ++ " | "
            ++ (show $ token|>tCode) ++ ": " ++ (escapeString $ token|>tText) ++ "\n"

-- | @escapeString string@ escapes all the non-ASCII characters in the
-- /string/, as well as escaping the \"@\\@\" character, using the \"@\\xXX@\",
-- \"@\\uXXXX@\" and \"@\\UXXXXXXXX@\" escape sequences.
escapeString :: String -> String
escapeString []                                   = []
escapeString (first:rest)
  | ' ' <= first && first /= '\\' && first <= '~' = first:(escapeString rest)
  | first <= '\xFF'                               = "\\x" ++ (toHex 2 $ ord first) ++ (escapeString rest)
  | '\xFF' < first && first <= '\xFFFF'           = "\\u" ++ (toHex 4 $ ord first) ++ (escapeString rest)
  | otherwise                                     = "\\U" ++ (toHex 8 $ ord first) ++ (escapeString rest)

-- | @toHex digits int@ converts the /int/ to the specified number of
-- hexadecimal /digits/.
toHex :: Int -> Int -> String
toHex digits int
  | digits > 1  = (toHex (digits .- 1) (int `div` 16)) ++ [intToDigit $ int `mod` 16]
  | digits == 1 = [intToDigit int]

-- | @showTokens tokens@ converts a list of /tokens/ to a multi-line YEAST
-- text.
showTokens :: [Token] -> String
showTokens tokens = foldr (\ token text -> (show token) ++ text) "" tokens

-- * Parsing framework
--
-- Haskell has no shortage of parsing frameworks. We use our own because:
--
--  * Most available frameworks are inappropriate because of their focus on
--    building a parse tree, and completing all of it before any of it is
--    accessible to the caller. We return a stream of tokens, and would like
--    its head to be accessible as soon as possible to allow for streaming. To
--    do this with bounded memory usage we use a combination of continuation
--    passing style and difference lists for the collected tokens.
--
--  * Haskell makes it so easy to roll your own parsing framework. We need some
--    specialized machinery (limited lookahead, forbidden patterns). It is
--    possible to build these on top of existing frameworks but the end result
--    isn't much shorter than rolling our own.
--
-- Since we roll our own framework we don't bother with making it generalized,
-- so we maintain a single 'State' type rather than having a generic one that
-- contains a polymorphic \"UserState\" field etc.
-- Showing a 'State' is only used in debugging.
instance (Show result) => Show (Reply result) where
  show reply = "Result: "    ++ (show $ reply|>rResult)
            ++ ", Tokens: "  ++ (show $ D.toList $ reply|>rTokens)
            ++ ", Commit: "  ++ (show $ reply|>rCommit)
            ++ ", State: { " ++ (show $ reply|>rState) ++ "}"

-- Showing a 'State' is only used in debugging. Note that forcing dump of
-- @sInput@ will disable streaming it.
instance Show State where
  show state = "Name: "              ++ (show $ state|>sName)
            ++ ", Decision: "        ++ (show $ state|>sDecision)
            ++ ", Limit: "           ++ (show $ state|>sLimit)
            ++ ", IsPeek: "          ++ (show $ state|>sIsPeek)
            ++ ", IsSol: "           ++ (show $ state|>sIsSol)
            ++ ", Chars: >>>"        ++ (reverse $ state|>sChars) ++ "<<<"
            ++ ", CharsCharOffset: " ++ (show $ state|>sCharsCharOffset)
            ++ ", CharsLine: "       ++ (show $ state|>sCharsLine)
            ++ ", CharsLineChar: "   ++ (show $ state|>sCharsLineChar)
            ++ ", CharOffset: "      ++ (show $ state|>sCharOffset)
            ++ ", Line: "            ++ (show $ state|>sLine)
            ++ ", LineChar: "        ++ (show $ state|>sLineChar)
            ++ ", Code: "            ++ (show $ state|>sCode)
            ++ ", Last: "            ++ (show $ state|>sLast)
            ++ ", Input: >>>"        ++ (show $ state|>sInput) ++ "<<<"

-- | @initialState name input@ returns an initial 'State' for parsing the
-- /input/ (with /name/ for error messages).
initialState :: String -> String -> State
initialState name input =    State { sName            = name,
                                     sDecision        = "",
                                     sLimit           = -1,
                                     sForbidden       = Nothing,
                                     sIsPeek          = False,
                                     sIsSol           = True,
                                     sChars           = [],
                                     sCharsCharOffset = -1,
                                     sCharsLine       = -1,
                                     sCharsLineChar   = -1,
                                     sCharOffset      = 0,
                                     sLine            = 1,
                                     sLineChar        = 0,
                                     sCode            = Unparsed,
                                     sLast            = ' ',
                                     sInput           = input   }

-- *** Setters
--
-- We need four setter functions to pass them around as arguments. For some
-- reason, Haskell only generates getter functions.

-- | @setDecision name state@ sets the @sDecision@ field to /decision/.
setDecision :: String -> State -> State
setDecision decision state = state { sDecision = decision }

-- | @setLimit limit state@ sets the @sLimit@ field to /limit/.
setLimit :: Int -> State -> State
setLimit limit state = state { sLimit = limit }

-- | @setForbidden forbidden state@ sets the @sForbidden@ field to /forbidden/.
setForbidden :: Maybe Pattern -> State -> State
setForbidden forbidden state = state { sForbidden = forbidden }

-- | @setCode code state@ sets the @sCode@ field to /code/.
setCode :: Code -> State -> State
setCode code state = state { sCode = code }

-- ** Implicit parsers
--
-- It is tedious to have to wrap each expected character (or character range)
-- in an explicit 'Parse' constructor. We let Haskell do that for us using a
-- 'Match' class.

-- | @Match parameter result@ specifies that we can convert the /parameter/ to
-- a 'Parser' returning the /result/.
class Match parameter result | parameter -> result where
    match :: parameter -> Parser result

-- | We don't need to convert a 'Parser', it already is one.
instance Match (Parser result) result where
    match = id

-- | We convert 'Char' to a parser for a character (that returns nothing).
instance Match Char () where
    match code = nextIf (== code)

-- | We convert a 'Char' tuple to a parser for a character range (that returns
-- nothing).
instance Match (Char, Char) () where
    match (low, high) = nextIf $ \ code -> low <= code && code <= high

-- | We convert 'String' to a parser for a sequence of characters (that returns
-- nothing).
instance Match String () where
    match = foldr (&) empty

-- ** Reply constructors

-- | @returnReply state result@ prepares a 'Reply' with the specified /state/
-- and /result/.
returnReply :: State -> result -> Reply result
returnReply state result = Reply { rResult = Result result,
                                   rTokens = D.empty,
                                   rCommit = Nothing,
                                   rState  = state }

-- | @tokenReply state token@ returns a 'Reply' containing the /state/ and
-- /token/. Any collected characters are cleared (either there are none, or we
-- put them in this token, or we don't want them).
tokenReply state token = Reply { rResult = Result (),
                                 rTokens = D.singleton token,
                                 rCommit = Nothing,
                                 rState  = state { sCharsCharOffset = -1,
                                                   sCharsLine       = -1,
                                                   sCharsLineChar   = -1,
                                                   sChars           = [] } }

-- | @failReply state message@ prepares a 'Reply' with the specified /state/
-- and error /message/.
failReply :: State -> String -> Reply result
failReply state message = Reply { rResult = Failed message,
                                  rTokens = D.empty,
                                  rCommit = Nothing,
                                  rState  = state }

-- | @unexpectedReply state@ returns a @failReply@ for an unexpected character.
unexpectedReply :: State -> Reply result
unexpectedReply state = case state|>sInput of
                             (char:_) -> failReply state $ "Unexpected '" ++ [char] ++ "'"
                             []            -> failReply state "Unexpected end of input"

-- | Allow using the @do@ notation for our parsers, which makes for short and
-- sweet @do@ syntax when we want to examine the results (we typically don't).
instance Monad Parser where

  -- @return result@ does just that - return a /result/.
  return result = Parser $ \ state -> returnReply state result

  -- @left >>= right@ applies the /left/ parser, and if it didn't fail
  -- applies the /right/ one (well, the one /right/ returns).
  left >>= right = bindParser left right where
    bindParser (Parser left) right = Parser $ \ state ->
                       let reply = left state
                           state' = reply|>rState
                       in case reply|>rResult of
                               Failed message -> reply { rResult = Failed message }
                               Result value   -> reply { rResult = More $ right value }
                               More parser    -> reply { rResult = More $ bindParser parser right }

  -- @fail message@ does just that - fails with a /message/.
  fail message = Parser $ \ state -> failReply state message

-- ** Parsing operators
--
-- Here we reap the benefits of renaming the numerical operators. The Operator
-- precedence, in decreasing strength:
--
-- @repeated % n@, @repeated <% n@, @match - rejected@, @match ! decision@,
-- @match ?! decision@, @choice ^ (first \/ second)@.
--
-- @match - first - second@ is @(match - first) - second@.
--
-- @first & second & third@ is @first & (second & third)@. Note that @first -
-- rejected & second@ is @(first - rejected) & second@, etc.
--
-- @match \/ alternative \/ otherwise@ is @match \/ (alternative \/
-- otherwise)@. Note that @first & second \/ third@ is @(first & second) \/
-- third@.
--
-- @( match *)@, @(match +)@, @(match ?)@, @(match <?)@, @(match >?)@, @(match
-- >!)@, @(match <!)@ are the weakest and require the surrounding @()@.

infix  3 ^
infix  3 %
infix  3 <%
infix  3 !
infix  3 ?!
infixl 3 -
infixr 2 &
infixr 1 /
infix  0 ?
infix  0 *
infix  0 +
infix  0 <?
infix  0 >?
infix  0 <!
infix  0 >!

-- | @parser % n@ repeats /parser/ exactly /n/ times.
(%) :: (Match match result) => match -> Int -> Pattern
parser % n
  | n <= 0 = empty
  | n > 0  = parser & parser % n .- 1

-- | @parser <% n@ matches fewer than /n/ occurrences of /parser/.
(<%) :: (Match match result) => match -> Int -> Pattern
parser <% n
  | n < 1 = fail "Fewer than 0 repetitions"
  | n == 1 = reject parser Nothing
  | n > 1  = "<%" ^ ( parser ! "<%" & parser <% n .- 1 / empty )

-- | @decision ^ (option \/ option \/ ...)@ provides a /decision/ name to the
-- choice about to be made, to allow to @commit@ to it.
(^) :: (Match match result) => String -> match -> Parser result
decision ^ parser = choice decision $ match parser

-- | @parser ! decision@ commits to /decision/ (in an option) after
-- successfully matching the /parser/.
(!) :: (Match match result) => match -> String -> Pattern
parser ! decision = parser & commit decision

-- | @parser ?! decision@ commits to /decision/ (in an option) if the current
-- position matches /parser/, without consuming any characters.
(?!) :: (Match match result) => match -> String -> Pattern
parser ?! decision = peek parser & commit decision

-- | @lookbehind <?@ matches the current point without consuming any
-- characters, if the previous character matches the lookbehind parser (single
-- character positive lookbehind)
(<?) :: (Match match result) => match -> Parser result
(<?) lookbehind = prev lookbehind

-- | @lookahead >?@ matches the current point without consuming any characters
-- if it matches the lookahead parser (positive lookahead)
(>?) :: (Match match result) => match -> Parser result
(>?) lookahead = peek lookahead

-- | @lookbehind <?@ matches the current point without consuming any
-- characters, if the previous character does not match the lookbehind parser
-- (single character negative lookbehind)
(<!) :: (Match match result) => match -> Pattern
(<!) lookbehind = prev $ reject lookbehind Nothing

-- | @lookahead >?@ matches the current point without consuming any characters
-- if it matches the lookahead parser (negative lookahead)
(>!) :: (Match match result) => match -> Pattern
(>!) lookahead = reject lookahead Nothing

-- | @parser - rejected@ matches /parser/, except if /rejected/ matches at this
-- point.
(-) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result1
parser - rejected = reject rejected Nothing & parser

-- | @before & after@ parses /before/ and, if it succeeds, parses /after/. This
-- basically invokes the monad's @>>=@ (bind) method.
(&) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result2
before & after = (match before) >> (match after)

-- | @first \/ second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case is fails immediately.
(/) :: (Match match1 result, Match match2 result) => match1 -> match2 -> Parser result
first / second = Parser $ \ state ->
  let Parser parser = decide (match first) (match second)
  in parser state

-- | @(optional ?)@ tries to match /parser/, otherwise does nothing.
(?) :: (Match match result) => match -> Pattern
(?) optional = (optional & empty) / empty

-- | @(parser *)@ matches zero or more occurrences of /repeat/, as long as each
-- one actually consumes input characters.
(*) :: (Match match result) => match -> Pattern
(*) parser = "*" ^ zomParser
  where zomParser = (parser ! "*" & zomParser) / empty

-- | @(parser +)@ matches one or more occurrences of /parser/, as long as each
-- one actually consumed input characters.
(+) :: (Match match result) => match -> Pattern
(+) parser = parser & (parser *)

-- ** Basic parsers

-- | @decide first second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case is fails immediately.
decide :: Parser result -> Parser result -> Parser result
decide left right = Parser $ \ state ->
  let Parser parser = decideParser state D.empty left right
  in parser state
  where decideParser point tokens (Parser left) right = Parser $ \state ->
          let reply = left state
              tokens' reply = D.append tokens $ reply|>rTokens
          in case (reply|>rResult, reply|>rCommit) of
                  (Failed _,    _)      -> Reply { rState  = point,
                                                   rTokens = D.empty,
                                                   rResult = More right,
                                                   rCommit = Nothing }
                  (Result _,   _)       -> reply { rTokens = tokens' reply }
                  (More left', Just _)  -> reply { rTokens = tokens' reply,
                                                   rResult = More left' }
                  (More left', Nothing) -> let Parser parser = decideParser point (tokens' reply) left' right
                                           in parser $ reply|>rState

-- | @choice decision parser@ provides a /decision/ name to the choice about to
-- be made in /parser/, to allow to @commit@ to it.
choice :: String -> Parser result -> Parser result
choice decision parser = Parser $ \ state ->
  let Parser parser' = choiceParser (state|>sDecision) decision parser
  in parser' state { sDecision = decision }
  where choiceParser parentDecision makingDecision (Parser parser) = Parser $ \ state ->
          let reply   = parser state
              commit' = case reply|>rCommit of
                             Nothing                                    -> Nothing
                             Just decision | decision == makingDecision -> Nothing
                                           | otherwise                  -> reply|>rCommit
              reply'  = case reply|>rResult of
                             More parser' -> reply { rCommit = commit',
                                                     rResult = More $ choiceParser parentDecision makingDecision parser' }
                             _            -> reply { rCommit = commit',
                                                     rState = (reply|>rState) { sDecision = parentDecision } }
          in reply'

-- | @parser ``recovery`` pattern@ parses the specified /parser/; if it fails,
-- it continues to the /recovery/ parser to recover.
recovery :: (Match match1 result, Match match2 result) => match1 -> match2 -> Parser result
recovery pattern recover =
  Parser $ \ state ->
    let (Parser parser) = match pattern
        reply = parser state
    in if state|>sIsPeek
          then reply
          else case reply|>rResult of
                    Result _       -> reply
                    More more      -> reply { rResult = More $ more `recovery` recover }
                    Failed message -> reply { rResult = More $ fake Error message & unparsed & recover }
    where unparsed = let (Parser parser) = match finishToken
                     in Parser $ \ state -> parser $ state { sCode = Unparsed }

-- | @prev parser@ succeeds if /parser/ matches at the previous character. It
-- does not consume any input.
prev :: (Match match result) => match -> Parser result
prev parser = Parser $ \ state ->
  prevParser state (match parser) state { sIsPeek = True, sInput = (state|>sLast) : state|>sInput }
  where prevParser point (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> prevParser point parser' $ reply|>rState

-- | @peek parser@ succeeds if /parser/ matches at this point, but does not
-- consume any input.
peek :: (Match match result) => match -> Parser result
peek parser = Parser $ \ state ->
  peekParser state (match parser) state { sIsPeek = True }
  where peekParser point (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> peekParser point parser' $ reply|>rState

-- | @reject parser name@ fails if /parser/ matches at this point, and does
-- nothing otherwise. If /name/ is provided, it is used in the error message,
-- otherwise the messages uses the current character.
reject :: (Match match result) => match -> Maybe String -> Pattern
reject parser name = Parser $ \ state ->
  rejectParser state name (match parser) state { sIsPeek = True }
  where rejectParser point name (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> returnReply point ()
                  Result value   -> case name of
                                         Nothing   -> unexpectedReply point
                                         Just text -> failReply point $ "Unexpected " ++ text
                  More parser'   -> rejectParser point name parser' $ reply|>rState

-- | @upto parser@ consumes all the character up to and not including the next
-- point where the specified parser is a match.
upto :: Pattern -> Pattern

upto parser = ( ( parser >!) & nextIf (const True) *)

-- | @nonEmpty parser@ succeeds if /parser/ matches some non-empty input
-- characters at this point.
nonEmpty :: (Match match result) => match -> Parser result
nonEmpty parser = Parser $ \ state ->
  let Parser parser' = nonEmptyParser (state|>sCharOffset) (match parser)
  in parser' state
  where nonEmptyParser offset (Parser parser) = Parser $ \ state ->
          let reply = parser state
              state' = reply|>rState
          in case reply|>rResult of
                  Failed message -> reply
                  Result value   -> if state'|>sCharOffset > offset
                                       then reply
                                       else failReply state' "Matched empty pattern"
                  More parser'   -> reply { rResult = More $ nonEmptyParser offset parser' }

-- | @empty@ always matches without consuming any input.
empty :: Pattern
empty = return ()

-- | @eof@ matches the end of the input.
eof :: Pattern
eof = Parser $ \ state ->
  if state|>sInput == []
     then returnReply state ()
     else unexpectedReply state

-- | @sol@ matches the start of a line.
sol :: Pattern
sol = Parser $ \ state ->
  if state|>sIsSol
     then returnReply state ()
     else failReply state "Expected start of line"

-- ** State manipulation pseudo-parsers

-- | @commit decision@ commits the parser to all the decisions up to the most
-- recent parent /decision/. This makes all tokens generated in this parsing
-- path immediately available to the caller.
commit :: String -> Pattern
commit decision = Parser $ \ state ->
  Reply { rState  = state,
          rTokens = D.empty,
          rResult = Result (),
          rCommit = Just decision }

-- | @nextLine@ increments @sLine@ counter and resets @sLineChar@.
nextLine :: Pattern
nextLine = Parser $ \ state ->
  returnReply state { sIsSol    = True,
                      sLine     = state|>sLine .+ 1,
                      sLineChar = 0 }
              ()

-- | @with setField getField value parser@ invokes the specified /parser/ with
-- the value of the specified field set to /value/ for the duration of the
-- invocation, using the /setField/ and /getField/ functions to manipulate it.
with :: (value -> State -> State) -> (State -> value) -> value -> Parser result -> Parser result
with setField getField value parser = Parser $ \ state ->
  let value' = getField state
      Parser parser' = value' `seq` withParser value' parser
  in parser' $ setField value state
  where withParser parentValue (Parser parser) = Parser $ \ state ->
          let reply = parser state
          in case reply|>rResult of
                  Failed _     -> reply { rState = setField parentValue $ reply|>rState }
                  Result _     -> reply { rState = setField parentValue $ reply|>rState }
                  More parser' -> reply { rResult = More $ withParser parentValue parser' }

-- | @parser ``forbidding`` pattern@ parses the specified /parser/ ensuring
-- that it does not contain anything matching the /forbidden/ parser.
forbidding :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result1
forbidding parser forbidden = with setForbidden sForbidden (Just $ forbidden & empty) (match parser)

-- | @parser ``limitedTo`` limit@ parses the specified /parser/
-- ensuring that it does not consume more than the /limit/ input chars.
limitedTo :: (Match match result) => match -> Int -> Parser result
limitedTo parser limit = with setLimit sLimit limit (match parser)

-- ** Consuming input characters

-- | @nextIf test@ fails if the current position matches the 'State' forbidden
-- pattern or if the 'State' lookahead limit is reached. Otherwise it consumes
-- (and buffers) the next input char if it satisfies /test/.
nextIf :: (Char -> Bool) -> Pattern
nextIf test = Parser $ \ state ->
  case state|>sForbidden of
       Nothing     -> limitedNextIf state
       Just parser -> let Parser parser' = reject parser $ Just "forbidden pattern"
                          reply = parser' state { sForbidden = Nothing }
                      in case reply|>rResult of
                              Failed _ -> reply
                              Result _ -> limitedNextIf state
  where limitedNextIf state =
          case state|>sLimit of
               -1    -> consumeNextIf state
               0     -> failReply state "Lookahead limit reached"
               limit -> consumeNextIf state { sLimit = state|>sLimit .- 1 }
        consumeNextIf state =
          case state|>sInput of
               (char:rest) | test char -> let chars = if state|>sIsPeek
                                                                   then []
                                                                   else char:(state|>sChars)
                                              char_offset = charsOf sCharOffset sCharsCharOffset
                                              line        = charsOf sLine       sCharsLine
                                              line_char   = charsOf sLineChar   sCharsLineChar
                                              is_sol = if char == '\xFEFF'
                                                                    then state|>sIsSol
                                                                    else False
                                              state' = state { sInput           = rest,
                                                               sLast            = char,
                                                               sChars           = chars,
                                                               sCharsCharOffset = char_offset,
                                                               sCharsLine       = line,
                                                               sCharsLineChar   = line_char,
                                                               sIsSol           = is_sol,
                                                               sCharOffset      = state|>sCharOffset .+ 1,
                                                               sLineChar        = state|>sLineChar .+ 1 }
                                                    in returnReply state' ()
                           | otherwise -> unexpectedReply state
               []                      -> unexpectedReply state

          where charsOf field charsField = if state|>sIsPeek
                                              then -1
                                              else if state|>sChars == []
                                                      then state|>field
                                                      else state|>charsField

-- ** Producing tokens

-- | @finishToken@ places all collected text into a new token and begins a new
-- one, or does nothing if there are no collected characters.
finishToken :: Pattern
finishToken = Parser $ \ state ->
  let state' = state { sChars           = [],
                       sCharsCharOffset = -1,
                       sCharsLine       = -1,
                       sCharsLineChar   = -1 }
  in if state|>sIsPeek
        then returnReply state' ()
        else case state|>sChars of
                  []          -> returnReply state' ()
                  chars@(_:_) -> tokenReply state' Token { tCharOffset = state|>sCharsCharOffset,
                                                           tLine       = state|>sCharsLine,
                                                           tLineChar   = state|>sCharsLineChar,
                                                           tCode       = state|>sCode,
                                                           tText       = reverse chars }

-- | @wrap parser@ invokes the /parser/, ensures any unclaimed input characters
-- are wrapped into a token (only happens when testing productions), ensures no
-- input is left unparsed, and returns the parser's result.
wrap :: (Match match result) => match -> Parser result
wrap parser = do result <- match parser
                 finishToken
                 eof
                 return result

-- | @consume parser@ invokes the /parser/ and then consumes all remaining
-- unparsed input characters.
consume :: (Match match result) => match -> Parser result
consume parser = do result <- match parser
                    finishToken
                    clearInput
                    return result
                 where clearInput = Parser $ \ state -> returnReply state { sInput = [] } ()

-- | @token code parser@ places all text matched by /parser/ into a 'Token' with
-- the specified /code/ (unless it is empty). Note it collects the text even if
-- there is an error.
token :: (Match match result) => Code -> match -> Pattern
token code parser = finishToken & with setCode sCode code (parser & finishToken)

-- | @fake code text@ creates a token with the specified /code/ and \"fake\"
-- /text/ characters, instead of whatever characters are collected so far.
fake :: Code -> String -> Pattern
fake code text = Parser $ \ state ->
  if state|>sIsPeek
     then returnReply state ()
     else tokenReply state Token { tCharOffset = value state sCharOffset sCharsCharOffset,
                                   tLine       = value state sLine sCharsLine,
                                   tLineChar   = value state sLineChar sCharsLineChar,
                                   tCode       = code,
                                   tText       = text }
    where value state field1 field2 =
            if field2 state == -1
               then field1 state
               else field2 state

-- | @meta parser@ collects the text matched by the specified /parser/ into a
-- | @Meta@ token.
meta :: (Match match result) => match -> Pattern
meta parser = token Meta parser

-- | @indicator code@ collects the text matched by the specified /parser/ into an
-- @Indicator@ token.
indicator :: (Match match result) => match -> Pattern
indicator parser = token Indicator $ parser

-- | @text parser@  collects the text matched by the specified /parser/ into a
-- @Text@ token.
text :: (Match match result) => match -> Pattern
text parser = token Text parser

-- | @emptyToken code@ returns an empty token.
emptyToken :: Code -> Pattern
emptyToken code = finishToken & parser code
  where parser code = Parser $ \ state ->
          if state|>sIsPeek
             then returnReply state ()
             else tokenReply state Token { tCharOffset = state|>sCharOffset,
                                           tLine       = state|>sLine,
                                           tLineChar   = state|>sLineChar,
                                           tCode       = code,
                                           tText       = "" }

-- | @wrapTokens beginCode endCode parser@ wraps the specified /parser/ with
-- matching /beginCode/ and /endCode/ tokens.
wrapTokens :: Code -> Code -> Pattern -> Pattern
wrapTokens beginCode endCode pattern = emptyToken beginCode
                                      & prefixErrorWith pattern (emptyToken endCode)
                                      & emptyToken endCode

-- | @prefixErrorWith pattern prefix@ will invoke the @prefix@ parser if an
-- error is detected during the @pattern@ parser, and then return the error.
prefixErrorWith :: (Match match result) => match -> Pattern -> Parser result
prefixErrorWith pattern prefix =
  Parser $ \ state ->
    let (Parser parser) = match pattern
        reply = parser state
    in case reply|>rResult of
            Result _       -> reply
            More more      -> reply { rResult = More $ prefixErrorWith more prefix }
            Failed message -> reply { rResult = More $ prefix & (fail message :: Parser result) }

-- * Production parameters

-- | Production context.
data Context = BlockOut     -- ^ Outside block sequence.
             | BlockIn      -- ^ Inside block sequence.
             | FlowOut      -- ^ Outside flow collection.
             | FlowIn       -- ^ Inside flow collection.
             | BlockKey     -- ^ Implicit block key.
             | FlowKey      -- ^ Implicit flow key.

-- | @show context@ converts a 'Context' to a 'String'.
instance Show Context where
  show context = case context of
                      BlockOut -> "block-out"
                      BlockIn  -> "block-in"
                      FlowOut  -> "flow-out"
                      FlowIn   -> "flow-in"
                      BlockKey -> "block-key"
                      FlowKey  -> "flow-key"

-- | @read context@ converts a 'String' to a 'Context'. We trust our callers to
-- convert any @-@ characters into @_@ to allow the built-in @lex@ function to
-- handle the names as single identifiers.
instance Read Context where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "block_out" -> BlockOut
                        "block_in"  -> BlockIn
                        "flow_out"  -> FlowOut
                        "flow_in"   -> FlowIn
                        "block_key" -> BlockKey
                        "flow_key"  -> FlowKey
                        _           -> error $ "unknown context: " ++ word

-- | Chomp method.
data Chomp = Strip -- ^ Remove all trailing line breaks.
           | Clip  -- ^ Keep first trailing line break.
           | Keep  -- ^ Keep all trailing line breaks.

-- | @show chomp@ converts a 'Chomp' to a 'String'.
instance Show Chomp where
  show chomp = case chomp of
                    Strip -> "strip"
                    Clip  -> "clip"
                    Keep  -> "keep"

-- | @read chomp@ converts a 'String' to a 'Chomp'.
instance Read Chomp where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "strip" -> Strip
                        "clip"  -> Clip
                        "keep"  -> Keep
                        _       -> error $ "unknown chomp: " ++ word

-- | @detect_utf_encoding@ doesn't actually detect the encoding, since decoding
-- has been delegated out to the enclosing Iteratee, report as if it was UTF-8.
bom code = code
         & (Parser $ \ state -> let text = "TF-8"
                                    Parser parser = fake Bom text
                                in  parser state)

-- | @na@ is the \"non-applicable\" indentation value. We use Haskell's laziness
-- to verify it really is never used.
na :: Int
na = error "Accessing non-applicable indentation"

-- | @asInteger@ returns the last consumed character, which is assumed to be a
-- decimal digit, as an integer.
asInteger :: Parser Int
asInteger = Parser $ \ state -> returnReply state $ ord (state|>sLast) .- 48

-- | @result value@ is the same as /return value/ except that we give the
-- Haskell type deduction the additional boost it needs to figure out this is
-- wrapped in a 'Parser'.
result :: result -> Parser result
result = return

-- | @errorTokens tokens state message withFollowing@ appends an @Error@ token
-- with the specified /message/ at the end of /tokens/, and if /withFollowing/
-- also appends the unparsed text following the error as a final @Unparsed@
-- token.
errorTokens tokens state message withFollowing =
    let tokens' = D.append tokens $ D.singleton Token { tCharOffset = state|>sCharOffset,
                                                        tLine       = state|>sLine,
                                                        tLineChar   = state|>sLineChar,
                                                        tCode       = Error,
                                                        tText       = message }
    in if withFollowing && state|>sInput /= []
       then D.append tokens' $ D.singleton Token { tCharOffset = state|>sCharOffset,
                                                   tLine       = state|>sLine,
                                                   tLineChar   = state|>sLineChar,
                                                   tCode       = Unparsed,
                                                   tText       = state|>sInput }
       else tokens'

-- | @commitBugs reply@ inserts an error token if a commit was made outside a
-- named choice. This should never happen outside tests.
commitBugs :: Reply result -> D.DList Token
commitBugs reply =
  let tokens = reply|>rTokens
      state = reply|>rState
  in case reply|>rCommit of
          Nothing     -> tokens
          Just commit -> D.append tokens $ D.singleton Token { tCharOffset = state|>sCharOffset,
                                                               tLine       = state|>sLine,
                                                               tLineChar   = state|>sLineChar,
                                                               tCode       = Error,
                                                               tText       = "Commit to '" ++ commit ++ "' was made outside it" }

-- | @stepTok@ performs a single step of tokenizing a portion of input stream
-- currently available, returning a triple of the result, possible continuation
-- parser, and the state. NB: this is not yet a truly step-by-step tokenizer;
-- feed a whole Yaml document to it.

stepTok :: (Parser a, State) -> (D.DList Token, Maybe (Parser a), State)

stepTok (Parser p, s) = 
  let reply = p s
      tokens = commitBugs reply
      state' = reply |> rState
  in  case reply |> rResult of
        Failed message -> (errorTokens tokens state' message True, Nothing, state')
        Result _ -> (tokens, Nothing, state')
        More p' -> (tokens, Just p', state')
        

-- | Produce an initial pair of parser and state to pass to 'stepTok'.

initTok :: String -> (Parser (), State)

initTok input = (p, s) where
  p = wrap l_yaml_stream
  s = initialState "<step>" input

-- | Loop the step tokenizer over the given input 'String' (the whole input will
-- be consumed) and return the list of tokens (including pissoble error tokens).

loopTok :: String -> [Token]

loopTok inp = D.toList $ loop D.empty (initTok inp) where
  loop atks ps = let (tks, mbp, st) = stepTok ps
                     tt = D.append atks tks
                 in  case mbp of
                       Nothing -> tt
                       Just p -> loop tt (p, st)

#include "Reference.bnf"


