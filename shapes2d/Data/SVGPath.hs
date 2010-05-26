------------------------------------------------------------------
-- |
-- Module      :  Data.SVGPath
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
--
--
-- Parser of SVG path syntax (see SVG spec, 8.3.1)
------------------------------------------------------------------

-- This module is based on Tillman Vogt's code found in the SVGFont
-- package.

module Data.SVGPath (
  PathCommand (..)
 ,pathFromString
 ,ptsFromString
 ,pathPoints
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char

-- | A data type to represent SVG path commands. Constructors with `abs' represent
-- capital letter commands (absolute movement), constructors with `rel' represent
-- small letter commands (relative movement). See the SVG spec for details.

data PathCommand = 
   M_abs (Float, Float) 
 | M_rel (Float, Float) 
 | Z 
 | L_abs (Float, Float) 
 | L_rel (Float, Float) 
 | H_abs Float 
 | H_rel Float 
 | V_abs Float 
 | V_rel Float 
 | C_abs (Float,Float,Float,Float,Float,Float) 
 | C_rel (Float,Float,Float,Float,Float,Float) 
 | S_abs (Float,Float,Float,Float) 
 | S_rel (Float,Float,Float,Float) 
 | Q_abs (Float,Float,Float,Float) 
 | Q_rel (Float,Float,Float,Float) 
 | T_abs (Float, Float) 
 | T_rel (Float, Float)
  deriving (Show)

-- | Parse a path string into a list of coordinate pairs (x, y). If there is parse error,
-- an empty list is returned.

ptsFromString :: (Float, Float) -> String -> [(Float, Float)]
ptsFromString pt s = case pathFromString s of
  Left _ -> []
  Right p -> pathPoints p pt

-- | Parse a path string into either a list of path commands or parse error message.

pathFromString :: String -> Either String [PathCommand]
pathFromString str = 
  case (parse path_path "" str) of
    Left err -> Left $ show err
    Right x  -> Right x

-- | Convert a list of path commands into a list of coordinate pairs. This function
-- does not make any distinction between straight and curved lines, taking care only of the
-- start and end point of each command.

pathPoints :: [PathCommand] -> (Float, Float) -> [(Float, Float)]
pathPoints [] _ = []
pathPoints (Z:_) _ = []
pathPoints (g:gs) (x0, y0) = pnt' : pathPoints gs pnt' where
  pnt' = (x', y')
  tplus (a, b) (c, d) = (a + c, b + d)
  (x', y') = case g of
    M_abs (x, y) -> (x, y)
    M_rel (x, y) -> (x, y) `tplus` (x0, y0)
    L_abs (x, y) -> (x, y)
    L_rel (x, y) -> (x, y) `tplus` (x0, y0)
    V_abs y      -> (x0, y)
    V_rel y      -> (x0, y0 + y)
    H_abs x      -> (x, y0)
    H_rel x      -> (x0 + x, y0)
    C_abs (_, _, _, _, x, y) -> (x, y)
    C_rel (_, _, _, _, x, y) -> (x, y) `tplus` (x0, y0)
    Q_abs (_, _, x, y) -> (x, y)
    Q_rel (_, _, x, y) -> (x, y) `tplus` (x0, y0)
    T_abs (x, y) -> (x, y)
    T_rel (x, y) -> (x, y) `tplus` (x0, y0)
    _ -> (x0, y0)

-- Helper types (not exported).

type Svg_path = [(String, String, String, String)]

-- Utility functions (not exported).

symsym cmd prsr gcmd = do
  symbol cmd
  l <- many1 prsr
  return $ map gcmd l

path_element :: Parser [PathCommand]
path_element = whiteSpace >> ( 
  symsym "M" tuple2 M_abs <|>
  symsym "m" tuple2 M_rel <|>
  do { symbol "z"; return [Z]; } <|>
  symsym "L" tuple2 L_abs <|>
  symsym "l" tuple2 L_rel <|>
  symsym "H" integer (H_abs . fromIntegral) <|>
  symsym "h" integer (H_rel . fromIntegral) <|>
  symsym "V" integer (V_abs . fromIntegral) <|>
  symsym "v" integer (V_rel . fromIntegral) <|>
  symsym "T" tuple2 T_abs <|>
  symsym "t" tuple2 T_rel <|>
  symsym "C" tuple6 C_abs <|>
  symsym "c" tuple6 C_rel <|>
  symsym "S" tuple4 S_abs <|>
  symsym "s" tuple4 S_rel <|>
  symsym "Q" tuple4 Q_abs <|>
  symsym "q" tuple4 Q_rel)

path_path :: Parser [PathCommand]
path_path = do
  whiteSpace
  l <- many1 path_element
  eof
  return (concat l)

spaces = skipMany (space <|> char ',')

tuple2 :: Parser (Float,Float)
tuple2 = do 
  x <- myfloat 
  spaces 
  y <- myfloat 
  spaces
  return (realToFrac x, realToFrac y)


tuple4 :: Parser (Float,Float,Float,Float)
tuple4 = do 
  x1 <- myfloat 
  spaces
  y1 <- myfloat
  spaces
  x <- myfloat
  spaces
  y <- myfloat
  spaces
  return (realToFrac x1, realToFrac y1, realToFrac x, realToFrac y)


tuple6 :: Parser (Float,Float,Float,Float,Float,Float)
tuple6 = do 
  x1 <- myfloat
  spaces
  y1 <- myfloat
  spaces
  x2 <- myfloat
  spaces
  y2 <- myfloat
  spaces
  x <- myfloat
  spaces
  y <- myfloat
  spaces
  return (realToFrac x1, realToFrac y1, realToFrac x2, realToFrac y2, realToFrac x, realToFrac y)


myfloat = 
  try (do{ symbol "-"; n <- float; return (negate n) }) <|>
  try float <|> -- 0 is not recognized as a float, so recognize it as an 
                -- integer and then convert it to float
  do { i<-integer; return(fromIntegral i) } 

lexer = P.makeTokenParser oDef
oDef  = javaStyle {
  P.commentLine    = "#"
}

whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
integer         = P.integer lexer    
float           = P.float lexer

 
