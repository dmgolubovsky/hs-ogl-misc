-- Runner for the ns-base applications.

module Main where

import System.Environment
import System.IO9.NameSpaceT
import Text.Yaml.EnumTok
import qualified Data.DList as D
import qualified Data.Map as M

main = do
  args <- getArgs
  nsInit M.empty [] $ do
    dbgPrint "Whole text\n"
    mapM (dbgPrint . show) $ loopTok (concat yaml)
    return ()

yaml = ["---\n"
       ,"builtin: ls\n"
       ,"...\n"]

