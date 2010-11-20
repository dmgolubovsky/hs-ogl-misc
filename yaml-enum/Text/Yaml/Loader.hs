------------------------------------------------------------------
-- |
-- Module      :  Text.Yaml.Loader
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  LGPL (by inheritance)
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- YAML loader from a list of tokens.
------------------------------------------------------------------

module Text.Yaml.Loader (
  YamlTag (..)
 ,YamlAnchor (..)
 ,YamlNode (..)
 ,YamlElem (..)
 ,loadYaml
) where

import Text.Yaml.Types
import Text.Yaml.EnumTok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

-- | Yaml tag definition (borrowed from HsSyck).

type YamlTag = Maybe String

-- | Yaml node anchor definition (borrowed from HsSyck).

data YamlAnchor
    = AAnchor    !Int
    | AReference !Int
    | ASingleton
    deriving (Show, Ord, Eq)

-- | Yaml node definition (borrowed from HsSyck).

data YamlNode = MkNode { 
  n_elem      :: !YamlElem
 ,n_tag       :: !YamlTag
 ,n_anchor    :: !YamlAnchor}
  deriving (Show, Ord, Eq)

-- | Yaml element definition (borrowed from HsSyck).

data YamlElem
    = EDocument [YamlNode]
    | EMap [(YamlNode, YamlNode)]
    | ESeq [YamlNode]
    | EStr String
    | ENil
    deriving (Show, Ord, Eq)

-- | Yaml token stream parser.

type YP a = GenParser Token () a

-- | Token position.

tokPos :: Token -> SourcePos

tokPos t = newPos "" (tLine t) (tLineChar t)

-- | Parse the whole Yaml token list yielding one or more documents.

loadYaml :: [Token] -> Either ParseError [YamlElem]

loadYaml = parse (nextdoc >> many ydoc) ""

-- Generic token recognizer

ytoken = token (show . tCode) tokPos

-- Parse a whole document.

ydoc :: YP YamlElem

ydoc = do
  begindoc
  manyTill anyToken enddoc
  nextdoc
  return $ EDocument [MkNode ENil Nothing ASingleton]

-- Individual token recognizers.

begindoc = ytoken $ \t -> case tCode t of
  BeginDocument -> Just ()
  _ -> Nothing

enddoc = ytoken $ \t -> case tCode t of
  EndDocument -> Just ()
  _ -> Nothing

nextdoc = skipAhead begindoc

-- | Skip until the given parser succeeds, but only look that parser ahead.

skipAhead p = do
  z <- option 0 ((eof >> return 1) <|> (lookAhead p >> return 1))
  case z of
    0 -> anyToken >> nextdoc
    1 -> return ()

