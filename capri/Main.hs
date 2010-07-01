------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
-- 
--
--
-- Main module of capri, wrapper for cabal-install private mode
------------------------------------------------------------------

module Main where

import qualified Paths_capri
import Data.List
import System.Environment
import Distribution.Text
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Command

main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = topHandler $
  case commandsRun capriGlobalCommand commands args of
    CommandHelp   help                 -> printGlobalHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalflags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion globalflags)        -> printVersion
          | fromFlag (globalNumericVersion globalflags) -> printNumericVersion
        CommandHelp     help           -> printCommandHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action globalflags
  where
     printCommandHelp help = do
       pname <- getProgName
       putStr (help pname)
     printGlobalHelp help = do
       pname <- getProgName
       putStr (help pname)
     printOptionsList = putStr . unlines
     printErrors errs = die $ concat (intersperse "\n" errs)
     printNumericVersion = putStrLn $ display Paths_capri.version
     printVersion        = putStrLn $ "capri version "
                                   ++ display Paths_capri.version
                                   ++ "\nusing version "
                                   ++ display cabalVersion
                                   ++ " of the Cabal library "

commands = []

capriGlobalCommand :: CommandUI GlobalFlags
capriGlobalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandUsage        = \_ ->
         "This program provides a wrapper over cabal-install "
      ++ "to operate in project-private mode.\n",
    commandDescription  = Just $ \pname ->
         "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "Typical steps for installing Cabal packages:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["bootstrap", "clone", "configure", "build", "install"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         globalVersion (\v flags -> flags { globalVersion = v })
         trueArg
      ,option [] ["numeric-version"]
         "Print just the version number"
         globalNumericVersion (\v flags -> flags { globalNumericVersion = v })
         trueArg
      ]
  }



