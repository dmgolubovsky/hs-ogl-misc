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
import Data.Maybe
import System.IO
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

bootpkgs = ["ffi", "rts", "ghc-prim", "integer-gmp", "base-4*"]

subdir = ".capri"
pkgdir = subdir </> "packages"
instdir = subdir </> "install"

commands = [bootstrapCommand `commandAddAction` bootstrapAction
           ,listCommand `commandAddAction` listAction
           ,cloneCommand `commandAddAction` cloneAction
           ,ghcpkgCommand `commandAddAction` ghcpkgAction
           ,cabalCommand `commandAddAction` cabalAction]

-- Invoke cabal to run arbitrary action as a private process.

cabalCommand :: CommandUI ()

cabalCommand = CommandUI {
  commandName         = "cabal"
 ,commandSynopsis     = "Invoke the cabal-install program to run arbitrary " ++
                        "action on private packages"
 ,commandUsage        = (++ " cabal -- <cabal options>")
 ,commandDescription  = Just $ \pname -> "Use this command to invoke arbitrary action of " ++
                                         "cabal-install on the privately installed packages.\n" ++
                                         "Use double-hyphen (--) before any command parameters " ++
                                         "to avoid interpretation of them by " ++ pname ++ "\n\n"
 ,commandDefaultFlags = ()
 ,commandOptions = const []
}

cabalAction f s g = do
  cd <- getCurrentDirectory
  let dbopt = "--package-db=" ++ (cd </> pkgdir)
      pfxopt = "--prefix=" ++ (cd </> instdir)
  (_, _, _, p) <- privateProcessRaw "cabal" (s {- ++ [dbopt, pfxopt] -}) >>= createProcess
  waitForProcess p
  return ()

-- Invoke ghc-pkg to run arbitrary action as a private process.

ghcpkgCommand :: CommandUI ()

ghcpkgCommand = CommandUI {
  commandName         = "ghc-pkg"
 ,commandSynopsis     = "Invoke the ghc-pkg program to run arbitrary action on private packages"
 ,commandUsage        = (++ " ghc-pkg -- <ghc-pkg options>")
 ,commandDescription  = Just $ \pname -> "Use this command to invoke arbitrary action of " ++
                                         "ghc-pkg on the privately installed packages.\n" ++
                                         "Use double-hyphen (--) before any command parameters " ++
                                         "to avoid interpretation of them by " ++ pname ++ "\n\n"
 ,commandDefaultFlags = ()
 ,commandOptions = const []
}

ghcpkgAction f s g = do
  (_, _, _, p) <- privateProcessRaw "ghc-pkg" s >>= createProcess
  waitForProcess p
  return ()

-- Clone a package installed publicly into the private package database.
-- Exactly this action is performed by the bootstrap command on certain
-- essential packages. Note that --force option will not be passed to
-- ghc-pkg, so if some dependencies are missing, this has to be manually resolved.

cloneCommand :: CommandUI ()

cloneCommand = CommandUI {
  commandName         = "clone"
 ,commandSynopsis     = "Clone package(s) installed publicly into the private packages database"
 ,commandUsage        = (++ " clone <package name> [...<package name>]")
 ,commandDescription  = Just $ \pname -> "Use this command to install previously compiled " ++
                                         "package(s) from global \nor user database into the " ++
                                         "private database via running ghc-pkg.\n" ++
                                         "Package files (libraries, executables) will not be " ++
                                         "copied; \njust the description of the " ++ 
                                         "package will be transferred.\n" ++
                                         "Note that the --force flag will not be " ++
                                         "passed to ghc-pkg,\nso if any package dependencies " ++
                                         "are missing, cloning will not be completed\n" ++
                                         "It is a good idea to specify package " ++
                                         "version explicitly\n\n"
 ,commandDefaultFlags = ()
 ,commandOptions = const []
}

cloneAction f s g = do
  when (null s) $ die "at least one package name to clone should be specified"
  mapM clonePackage s
  return ()

-- List packages installed privately. Equivalent to running ghc-pkg list
-- as a "private" process.

listCommand :: CommandUI ()

listCommand = CommandUI {
  commandName         = "list"
 ,commandSynopsis     = "List packages installed privately"
 ,commandUsage        = (++ " list")
 ,commandDescription  = Just $ \pname -> "Use this command to list packages installed " ++
                                         "privately for this project.\n" ++ pname ++ " will " ++
                                         "run the ghc-pkg utility to perform this action\n\n"
 ,commandDefaultFlags = ()
 ,commandOptions = const []
}

listAction f s g = do
  (_, _, _, p) <- privateProcess "ghc-pkg list" >>= createProcess
  waitForProcess p
  return ()

-- Bootstrap a private per-project package configuration. Create a subdirectory .capri
-- where packages will be installed. Subdirectory .capri/packages will hold packages
-- cache and descriptions; subdirectory .capri/install will hold compiled libraries and
-- executables.

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

bootstrapAction f s g = do
  mapM createDirectory [subdir, instdir]
  ex <- runCommand ("ghc-pkg init " ++ pkgdir) >>= waitForProcess
  let exfail (ExitFailure _) = True
      exfail _ = False
  when (exfail ex) $ die "ghc-pkg failed to initialize package configuration:\n"
  mapM clonePackage bootpkgs
  return ()

-- Clone a package. This is same as running ghc-pkg describe as a "public" process,
-- and running ghc-pkg register as a "private" process.

clonePackage :: String -> IO ()

clonePackage pkg = do
  cwd <- getCurrentDirectory
  descr <- publicProcess ("ghc-pkg describe " ++ pkg) cwd
  regst <- privateProcess "ghc-pkg register -"
  (_, _, p) <- runPipe Inherit Inherit [descr, regst]
  mapM waitForProcess p
  return ()

-- Utility: create a process descriptor from given command operating in the project's
-- current directory and having environment variable GHC_PACKAGE_PATH set to
-- the project's private package storage. Standard I/O handles are set to Inherit,
-- but may be adjusted later when actually run the process.

privateProcess :: String -> IO CreateProcess
privateProcess = privateProcess' . ShellCommand

privateProcessRaw :: FilePath -> [String] -> IO CreateProcess
privateProcessRaw prog args = privateProcess' (RawCommand prog args)

privateProcess' cmd = do
  cd <- getCurrentDirectory
  env <- getEnvironment
  let env' = filter ((/= "GHC_PACKAGE_PATH") . fst) env
  return CreateProcess {
    cmdspec = cmd
   ,cwd = Just (cd </> subdir)
   ,env = Just (("GHC_PACKAGE_PATH", cd </> pkgdir):env')
   ,std_in = Inherit
   ,std_out = Inherit
   ,std_err = Inherit
   ,close_fds = False
  }

-- Utility: create a process descriptor to run a command without environment
-- adjustments, in given current directory.

publicProcess :: String -> FilePath -> IO CreateProcess

publicProcess cmd dir = return CreateProcess {
    cmdspec = ShellCommand cmd
   ,cwd = Just dir
   ,env = Nothing
   ,std_in = Inherit
   ,std_out = Inherit
   ,std_err = Inherit
   ,close_fds = False
}

-- Utility: pipe processes together. Two StdStream's must be supplied: for pipeline's
-- stdin and stdout. Stderr's of all processes are as set in the process creation
-- descriptors. List of process handles is returned as well as handles for pipe ends.

runPipe :: StdStream            -- stdin
        -> StdStream            -- stdout
        -> [CreateProcess]      -- processes (start at left, end at right)
        -> IO (Maybe Handle     -- pipe in
              ,Maybe Handle     -- pipe out
              ,[ProcessHandle]) -- same order as in the input list of descriptors

runPipe ins outs [] = return (Nothing, Nothing, [])

runPipe ins outs [p] = do
  let p' = p {std_in = ins, std_out = outs}
  (pin, pout, _, ph) <- createProcess p'
  return (pin, pout, [ph])

runPipe ins outs (pstart:ps) = do
  let pstart' = pstart {std_in = ins}
      prev@(pend:rps) = reverse (pstart':ps)
      pend' = pend {std_out = outs}
  piper (pend':rps) [] Nothing Nothing

piper :: [CreateProcess] 
      -> [ProcessHandle] 
      -> Maybe Handle
      -> Maybe Handle
      -> IO (Maybe Handle, Maybe Handle, [ProcessHandle])

piper [] phs pin pend = return (pin, pend, phs)

piper (p:ps) [] Nothing Nothing = do
  let p' = p {std_in = CreatePipe}
  (pin, pend, _, ph) <- createProcess p'
  piper ps [ph] pin pend

piper (p:ps) phs pin pend = do
  let p' = p {std_in = CreatePipe, std_out = UseHandle (fromJust pin)}
  (pin', _, _, ph) <- createProcess p'
  piper ps (ph:phs) pin' pend


