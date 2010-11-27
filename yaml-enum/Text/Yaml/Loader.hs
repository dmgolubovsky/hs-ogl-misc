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

import Numeric
import Data.Char
import Text.Yaml.Types
import Text.Yaml.EnumTok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Perm

-- | Yaml tag definition (borrowed from HsSyck).

type YamlTag = Maybe String

-- | Yaml node anchor definition (borrowed from HsSyck).

data YamlAnchor
    = AAnchor    String
    | AReference String
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
    | EError String
    deriving (Show, Ord, Eq)

-- | Yaml token stream parser.

type YP a = GenParser Token () a

-- | Token position.

tokPos :: Token -> SourcePos

tokPos t = newPos "" (tLine t) (tLineChar t)

-- | Parse the whole Yaml token list yielding one or more documents.
-- BUGS! BUGS! BUGS! Indents and breaks/folds are poorly handled, 
-- perhaps not at all...

loadYaml :: [Token] -> [YamlElem]

loadYaml y = case parse (bwi >> many ydoc) "" $ ycomment y of
  Left err -> [EError $ show err]
  Right es -> es

-- Generic token recognizers.

ytok c = (token (show . tCode) tokPos $ \t -> if tCode t == c then Just (tText t)
                                                              else Nothing) <?> (show c)

ycod c = (token (show . tCode) tokPos $ \t -> if tCode t == c then Just (tCode t)
                                                              else Nothing) <?> (show c)

yctx c x = (token (show . tCode) tokPos $ \t -> if tCode t == c  && tText t == x 
                                                  then Just (tCode t, tText t)
                                                  else Nothing) <?> (show c ++ " " ++ x)

-- Strip comments.

ycomment :: [Token] -> [Token]

ycomment [] = []
ycomment (t:ts) | tCode t == Bom = ycomment ts
ycomment (t:ts) | tCode t == BeginComment = yinc ts where
  yinc [] = []
  yinc (t:ts) | tCode t == EndComment = ycomment ts
  yinc (t:ts) = yinc ts
ycomment (t:ts) = t : (ycomment ts)

-- Parse a whole document.

ydoc :: YP YamlElem

ydoc = do
  ytok BeginDocument
  dirs <- many ydir
  option "" (ytok DirectivesEnd)
  ns <- manyTill ynode (ytok EndDocument)
  option "" (ytok DocumentEnd)
  bwi
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
  (anchor, tag) <- option (ASingleton, Nothing) yprops
  bwi
  c <- (ycod BeginScalar <|> ycod BeginMapping <|> ycod BeginSequence <|> ycod BeginAlias)
  r <- case c of
    BeginMapping -> do
      bwi
      option (Indicator, "") (yctx Indicator "{")
      bwi
      pairs <- many ypair
      bwi
      option (Indicator, "") (yctx Indicator "}")
      bwi
      ytok EndMapping
      bwi
      return $ MkNode (EMap pairs) tag anchor
    BeginScalar -> yscalar tag anchor
    BeginAlias -> yalias
    BeginSequence -> yseq tag anchor
    _ -> return $ MkNode ENil tag anchor
  ytok EndNode
  return r

yseq tag anchor = do
  bwi
  br <- option (Indicator, "") (yctx Indicator "[")
  els <- case (snd br) of
    "[" -> do
      ss <- sepBy ynode (bwi >> yctx Indicator "," >> bwi)
      yctx Indicator "]"
      return ss
    "" -> many $ do
      bwi
      yctx Indicator "-"
      bwi
      s <- ynode
      bwi
      return s
  ytok EndSequence
  return $ MkNode (ESeq els) tag anchor

yprops = between (ytok BeginProperties) (ytok EndProperties) $ permute $
  ((,) <$?> (ASingleton, yanchor) <|?> (Nothing, ytag))

yanchor = do
  ytok BeginAnchor
  yctx Indicator "&"
  m <- ytok Meta
  ytok EndAnchor
  bwi
  return $ AAnchor m

yalias = do
  yctx Indicator "*"
  m <- ytok Meta
  ytok EndAlias
  bwi
  return $ MkNode ENil Nothing (AReference m)


ytag = do
  ytok BeginTag
  bi <- (yctx BeginHandle "" <|> yctx Indicator "!")
  m <- case fst bi of
    BeginHandle -> do
      many (yctx Indicator "!")
      ytok EndHandle
      ytok Meta
    Indicator -> do
      yctx Indicator "<"
      m <- ytok Meta
      yctx Indicator ">"
      return m
  ytok EndTag
  bwi
  return $ Just m

yscalar tag anchor = do
  ifl <- option "|" (ytok Indicator)   -- indent/flow: | or >
  skc <- option "" (ytok Indicator)    -- chomp/keep/clip: - or + or blank
  t <- many ((yctx Indicator "\"" >> return "")
         <|> (yctx Indicator "\'" >> return "")
         <|> ytok Text 
         <|> ytok LineFold 
         <|> ytok LineFeed
         <|> ytok Indent
         <|> ytok White
         <|> (try yescape)
         <|> ytok Break) >>= return . concat
  ytok EndScalar
  bwi
  return $ MkNode (EStr t) tag anchor

yescape = do
  ytok BeginEscape
  yctx Indicator [chr 0x5c]
  m <- ytok Meta
  ytok EndEscape
  case m of
             "0" -> return [chr 0]
             "a" -> return [chr 7]
             "b" -> return [chr 8]
             "t" -> return [chr 9]
             "n" -> return "\n"
             "v" -> return [chr 0xB]
             "f" -> return [chr 0xC]
             "r" -> return [chr 0xD]
             "e" -> return [chr 0x1B]
             " " -> return " "
             "\"" -> return [chr 0x22]
             "/" -> return "/"
             "\\" -> return "\\"
             "N" -> return [chr 0x85]
             "_" -> return [chr 0xA0]
             "L" -> return [chr 0x202B]
             "P" -> return [chr 0x2029]
             'x':s | length s == 2 -> maybeRead s >>= return . (:[]) . chr
             'u':s | length s == 4 -> maybeRead s >>= return . (:[]) . chr
             'U':s | length s == 8 -> maybeRead s >>= return . (:[]) . chr
             _ -> fail $ "bad escape " ++ m

maybeRead :: (Num a, Read a, Monad m) => String -> m a
maybeRead s = 
  case readHex s of
    (a,_):_ -> return a
    _       -> fail "error in escape"


ypair = do
  bwi
  ytok BeginPair
  l <- ynode
  bwi
  option "" (ytok Indicator)
  bwi
  r <- ynode
  bwi
  ytok EndPair
  bwi
  option (Indicator, "") (yctx Indicator ",")
  bwi
  return (l, r)

bwi = skipMany (try (ytok Break) <|> (ytok Indent) <|> try (ytok White))


