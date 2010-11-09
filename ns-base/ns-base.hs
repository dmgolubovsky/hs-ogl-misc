-- Runner for the ns-base applications.

module Main where

import System.Environment
import System.IO9.NameSpaceT
import qualified Data.Map as M

main = do
  args <- getArgs
  nsInit M.empty [] $ do
    dbgPrint "ns-base\n"



