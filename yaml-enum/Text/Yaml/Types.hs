------------------------------------------------------------------
-- |
-- Module      :  Text.Yaml.Types
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
-- Enumerator/Iteratee compatible YAML tokenizer: type definitions
------------------------------------------------------------------

module Text.Yaml.Types where

import qualified Data.DList as D

-- * Result tokens
--
-- The parsing result is a stream of tokens rather than a parse tree. The idea
-- is to convert the YAML input into \"byte codes\". These byte codes are
-- intended to be written into a byte codes file (or more likely a UNIX pipe)
-- for further processing.

-- | 'Token' codes.
data Code = Bom             -- ^ BOM, contains \"@TF8@\", \"@TF16LE@\", \"@TF32BE@\", etc.
          | Text            -- ^ Content text characters.
          | Meta            -- ^ Non-content (meta) text characters.
          | Break           -- ^ Separation line break.
          | LineFeed        -- ^ Line break normalized to content line feed.
          | LineFold        -- ^ Line break folded to content space.
          | Indicator       -- ^ Character indicating structure.
          | White           -- ^ Separation white space.
          | Indent          -- ^ Indentation spaces.
          | DirectivesEnd   -- ^ Document start marker.
          | DocumentEnd     -- ^ Document end marker.
          | BeginEscape     -- ^ Begins escape sequence.
          | EndEscape       -- ^ Ends escape sequence.
          | BeginComment    -- ^ Begins comment.
          | EndComment      -- ^ Ends comment.
          | BeginDirective  -- ^ Begins directive.
          | EndDirective    -- ^ Ends directive.
          | BeginTag        -- ^ Begins tag.
          | EndTag          -- ^ Ends tag.
          | BeginHandle     -- ^ Begins tag handle.
          | EndHandle       -- ^ Ends tag handle.
          | BeginAnchor     -- ^ Begins anchor.
          | EndAnchor       -- ^ Ends anchor.
          | BeginProperties -- ^ Begins node properties.
          | EndProperties   -- ^ Ends node properties.
          | BeginAlias      -- ^ Begins alias.
          | EndAlias        -- ^ Ends alias.
          | BeginScalar     -- ^ Begins scalar content.
          | EndScalar       -- ^ Ends scalar content.
          | BeginSequence   -- ^ Begins sequence content.
          | EndSequence     -- ^ Ends sequence content.
          | BeginMapping    -- ^ Begins mapping content.
          | EndMapping      -- ^ Ends mapping content.
          | BeginPair       -- ^ Begins mapping key:value pair.
          | EndPair         -- ^ Ends mapping key:value pair.
          | BeginNode       -- ^ Begins complete node.
          | EndNode         -- ^ Ends complete node.
          | BeginDocument   -- ^ Begins document.
          | EndDocument     -- ^ Ends document.
          | BeginStream     -- ^ Begins YAML stream.
          | EndStream       -- ^ Ends YAML stream.
          | Error           -- ^ Parsing error at this point.
          | Unparsed        -- ^ Unparsed due to errors (or at end of test).
          | Detected        -- ^ Detected parameter (for testing).
  deriving (Eq, Show)

{-
-- | @show code@ converts a 'Code' to the one-character YEAST token code char.
-- The list of byte codes is also documented in the @yaml2yeast@ program.
instance Show Code where
  show code = case code of
                   Bom             -> "U"
                   Text            -> "T"
                   Meta            -> "t"
                   Break           -> "b"
                   LineFeed        -> "L"
                   LineFold        -> "l"
                   Indicator       -> "I"
                   White           -> "w"
                   Indent          -> "i"
                   DirectivesEnd   -> "K"
                   DocumentEnd     -> "k"
                   BeginEscape     -> "E"
                   EndEscape       -> "e"
                   BeginComment    -> "C"
                   EndComment      -> "c"
                   BeginDirective  -> "D"
                   EndDirective    -> "d"
                   BeginTag        -> "G"
                   EndTag          -> "g"
                   BeginHandle     -> "H"
                   EndHandle       -> "h"
                   BeginAnchor     -> "A"
                   EndAnchor       -> "a"
                   BeginProperties -> "P"
                   EndProperties   -> "p"
                   BeginAlias      -> "R"
                   EndAlias        -> "r"
                   BeginScalar     -> "S"
                   EndScalar       -> "s"
                   BeginSequence   -> "Q"
                   EndSequence     -> "q"
                   BeginMapping    -> "M"
                   EndMapping      -> "m"
                   BeginNode       -> "N"
                   EndNode         -> "n"
                   BeginPair       -> "X"
                   EndPair         -> "x"
                   BeginDocument   -> "O"
                   EndDocument     -> "o"
                   Error           -> "!"
                   Unparsed        -> "-"
                   Detected        -> "$"
-}
-- | Parsed token.
data Token = Token {
    tCharOffset :: Int,   -- ^ 0-base character offset in stream.
    tLine       :: Int,   -- ^ 1-based line number.
    tLineChar   :: Int,   -- ^ 0-based character in line.
    tCode       :: Code,  -- ^ Specific token 'Code'.
    tText       :: String -- ^ Contained input chars, if any.
  }

-- | A 'Parser' is basically a function computing a 'Reply'.
data Parser result = Parser (State -> Reply result)

-- | The 'Result' of each invocation is either an error, the actual result, or
-- a continuation for computing the actual result.
data Result result = Failed String        -- ^ Parsing aborted with a failure.
                   | Result result        -- ^ Parsing completed with a result.
                   | More (Parser result) -- ^ Parsing is ongoing with a continuation.

-- Showing a 'Result' is only used in debugging.
instance (Show result) => Show (Result result) where
  show result = case result of
                     Failed message -> "Failed " ++ message
                     Result result  -> "Result " ++ (show result)
                     More _         -> "More"

-- | Each invication of a 'Parser' yields a 'Reply'. The 'Result' is only one
-- part of the 'Reply'.
data Reply result = Reply {
    rResult :: !(Result result), -- ^ Parsing result.
    rTokens :: !(D.DList Token), -- ^ Tokens generated by the parser.
    rCommit :: !(Maybe String),  -- ^ Commitment to a decision point.
    rState  :: !State            -- ^ The updated parser state.
  }

-- ** Parsing state

-- | The internal parser state. We don't bother with parameterising it with a
-- \"UserState\", we just bundle the generic and specific fields together (not
-- that it is that easy to draw the line - is @sLine@ generic or specific?).
data State = State {
    sName            :: !String,          -- ^ The input name for error messages.
    sDecision        :: !String,          -- ^ Current decision name.
    sLimit           :: !Int,             -- ^ Lookahead characters limit.
    sForbidden       :: !(Maybe Pattern), -- ^ Pattern we must not enter into.
    sIsPeek          :: !Bool,            -- ^ Disables token generation.
    sIsSol           :: !Bool,            -- ^ Is at start of line?
    sChars           :: ![Char],          -- ^ (Reversed) characters collected for a token.
    sCharsCharOffset :: !Int,             -- ^ Char offset of first collected character.
    sCharsLine       :: !Int,             -- ^ Line of first collected character.
    sCharsLineChar   :: !Int,             -- ^ Character in line of first collected character.
    sCharOffset      :: !Int,             -- ^ Offset in characters in the input.
    sLine            :: !Int,             -- ^ Builds on YAML's line break definition.
    sLineChar        :: !Int,             -- ^ Character number in line.
    sCode            :: !Code,            -- ^ Of token we are collecting chars for.
    sLast            :: !Char,            -- ^ Last matched character.
    sInput           :: !String           -- ^ The decoded input characters.
  }

-- A 'Pattern' is a parser that doesn't have an (interesting) result.
type Pattern = Parser ()

