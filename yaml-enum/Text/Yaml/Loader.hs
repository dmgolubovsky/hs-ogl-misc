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
    = EDocument [(String, String)] [YamlNode]
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

loadYaml = parse (bwi >> many ydoc) ""

-- Generic token recognizers.

ytok c = (token (show . tCode) tokPos $ \t -> if tCode t == c then Just (tText t)
                                                              else Nothing) <?> (show c)

ycod c = (token (show . tCode) tokPos $ \t -> if tCode t == c then Just (tCode t)
                                                              else Nothing) <?> (show c)


-- Parse a whole document.

ydoc :: YP YamlElem

ydoc = do
  ytok BeginDocument
  dirs <- many ydir
  option "" (ytok DirectivesEnd)
  ns <- manyTill ynode (ytok EndDocument)
  return $ EDocument dirs ns

ydir = do
  ytok BeginDirective
  ytok Indicator
  s1 <- ytok Meta
  skipMany (ytok White)
  s2 <- ytok Meta
  ytok EndDirective
  bwi
  return (s1, s2)

ynode = do
  ytok BeginNode
  bwi
  c <- (ycod BeginScalar <|> ycod BeginMapping <|> ycod BeginSequence)
  r <- case c of
    BeginMapping -> do
      pairs <- many ypair
      ytok EndMapping
      return $ MkNode (EMap pairs) Nothing ASingleton
    BeginScalar -> yscalar
    _ -> return $ MkNode ENil Nothing ASingleton
  ytok EndNode
  return r

yscalar = do
  i <- option "|" (try $ ytok Indicator)
  t <- many (ytok Text 
         <|> ytok LineFold 
         <|> ytok LineFeed
         <|> ytok Indent
         <|> ytok White
         <|> ytok Break) >>= return . concat
  ytok EndScalar
  return $ MkNode (EStr t) Nothing ASingleton

ypair = do
  ytok BeginPair
  l <- ynode
  bwi
  ytok Indicator
  bwi
  r <- ynode
  skipMany (ytok Break)
  ytok EndPair
  return (l, r)

bwi = skipMany (try (ytok Break) <|> (ytok Indent) <|> try (ytok White))


