-- Repl with Hint


module Main where

import System.IO
import Control.Monad
import Data.List
import System.Exit
import System.Environment
import Language.Haskell.Interpreter

main :: IO ()

main = do
  putStrLn "repl"
  args <- getArgs
  when (null args) $ exitWith (ExitFailure 1)
  runInterpreter $ do
    reset
    evalStr $ "return $ show (" ++ head args ++ ")"
  hFlush stdout
  return () 

evalStr s = do
  r <- interpret s (as :: IO String)
  liftIO $ putStrLn =<< r

