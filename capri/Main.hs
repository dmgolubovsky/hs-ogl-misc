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
import System.Directory
import System.FilePath
import System.Process
import System.Environment
import System.Exit
import Control.Monad
import Distribution.Text
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Command

main :: IO ()
main = getArgs >>= mainWorker

-- Toplevel command handler. Any command not recognized in the provided list of
-- commands is forwarded to cabal-install as is, with proper environment set.

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
        CommandErrors   errs           -> processErrors args errs $ printErrors errs
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

-- Process errors as they possibly mean just a command for Cabal. Otherwise
-- do the default action.

processErrors :: [String] -> [String] -> IO () -> IO ()

processErrors args errs dflt = putStrLn (show errs) >> case errs of
  [] -> dflt
  (m:_) -> case words m of
    ("unrecognised":"command:":_) -> cabalInstall args
    _ -> dflt

-- (TODO) Run the cabal executable with proper environment settings.

cabalInstall :: [String] -> IO ()
cabalInstall args = 
  putStrLn $ "running Cabal with " ++ show args

-- Print global help message

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

bootpkgs = ["ffi", "rts", "integer-gmp", "base-4*"]

subdir = ".capri"
pkgdir = subdir </> "packages"
instdir = subdir </> "install"

commands = [bootstrapCommand `commandAddAction` bootstrapAction]

bootstrapCommand :: CommandUI ()

bootstrapCommand = CommandUI {
  commandName         = "bootstrap"
 ,commandSynopsis     = "Bootstrap private packages configuration"
 ,commandUsage        = (++ " bootstrap")
 ,commandDescription  = Just $ \pname -> "Use this command to create an empty per-project " ++
                                         "configuration of packages.\n" ++ pname ++ " will " ++
                                         "clone the following packages into the per-project " ++
                                         "packages directory: \n\n" ++
                                         concat (intersperse " " bootpkgs) ++ "\n\n"
 ,commandDefaultFlags = ()
 ,commandOptions = const []
}

-- Bootstrap a private per-project package configuration. Create a subdirectory .capri
-- where packages will be installed. Subdirectory .capri/packages will hold packages
-- cache and descriptions; subdirectory .capri/install will hold compiled libraries and
-- executables.

bootstrapAction f s g = do
  mapM createDirectory [subdir, instdir]
  ex <- runCommand ("ghc-pkg init " ++ pkgdir) >>= waitForProcess
  let exfail (ExitFailure _) = True
      exfail _ = False
  when (exfail ex) $ die "ghc-pkg failed to initialize package configuration:\n"
{-
  pp <- privateProcess "set | less"
  (_, _, _, p) <- createProcess pp
  waitForProcess p
-}  
  return ()

-- Utility: create a process descriptor from given command operating in the project's
-- current directory and having environment variable GHC_PACKAGE_PATH set to
-- the project's private package storage. Standard I/O handles are set to Inherit,
-- but may be adjusted later when actually run the process.

privateProcess :: String -> IO CreateProcess
privateProcess cmd = do
  cd <- getCurrentDirectory
  env <- getEnvironment
  let env' = filter ((/= "GHC_PACKAGE_PATH") . fst) env
  return CreateProcess {
    cmdspec = ShellCommand cmd
   ,cwd = Just (cd </> subdir)
   ,env = Just (("GHC_PACKAGE_PATH", pkgdir):env')
   ,std_in = Inherit
   ,std_out = Inherit
   ,std_err = Inherit
   ,close_fds = False
  }

