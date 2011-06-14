module Main where

import Data.Plastic.SymbolTrie
import Data.Plastic.Step

main :: IO ()

main = do
  putStrLn "Plastic"
  plus <- mkSymbol (\_ -> return 1) (\_ -> return ()) mkSymTrie "+:"
  minus <- mkSymbol (\_ -> return 1) (\_ -> return ()) mkSymTrie "-:"
  tern <- mkSymbol  (\_ -> return 1) (\_ -> return ()) mkSymTrie "takes:two:"
  let t = Local 1
  let code = Send (ValInt 1) plus [ValInt 2] .
             Send It minus [ValInt 3] .
             Store It t .
             Send (ValLoc t) plus [ValInt 4] .
             Send It tern [ValInt 5, ValDouble 4.2]
  putStrLn $ dis (code (Return (ValLoc t)))




