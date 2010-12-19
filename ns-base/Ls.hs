------------------------------------------------------------------
-- |
-- Module      :  Ls
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- ls: list contents of a directory.
------------------------------------------------------------------

module Ls (app) where

import Numeric
import System.IO9.Error
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import System.Console.CmdArgs
import Text.PrettyPrint
import Data.Bits
import Data.List
import Data.Maybe
import Data.NineP
import Data.NineP.Bits
import Data.NineP.Posix
import Data.Nesteratee
import Data.Enumerator (run, ($$))

app :: (MonadIO m, MonadCatchIO m) => AppTable m

app = appEntry [("ls", Monadic ls)]

data LsArgs = LsArgs {
  d :: Bool
 ,l :: Bool
 ,m :: Bool
 ,n :: Bool
 ,p :: Bool
 ,q :: Bool
 ,r :: Bool
 ,s :: Bool
 ,t :: Bool
 ,u :: Bool
 ,oF :: Bool
 ,oT :: Bool
 ,oQ :: Bool
 ,files :: [String]
} deriving (Data, Typeable, Show)

ls :: (MonadIO m, MonadCatchIO m) => [Argument] -> NameSpaceT m NineError

ls pargs = do
  (cas, reds) <- appCmdArgs pargs $ LsArgs {
        d = def &= help "if argument is a directory, list it, not its contents"
       ,l = def &= help "list in long format"
       ,m = def &= help "list the name of the user who most recently modified the file"
       ,n = def &= help "don't sort the listing"
       ,p = def &= help "print only the final path element of each file name"
       ,q = def &= help "list the qid of each file"
       ,r = def &= help "reverse the order of sort"
       ,s = def &= help "give size in Kbytes for each entry"
       ,t = def &= help "sort by time modified (latest first) instead of by name"
       ,u = def &= help "under -t sort by time of last access; under -l print time of last access"
       ,oF = def &= explicit &= name "F" &= help ("add the character / after all directory " ++ 
                                                  "names and the character * after all " ++ 
                                                  "executable files")
       ,oT = def &= explicit &= name "T" &= help ("print the character t before each file " ++
                                                  "if it has the temporary flag set, " ++
                                                  "and - otherwise")
       ,oQ = def &= explicit &= name "Q" &= help ("do not quote printed file names even " ++
                                                  "if they contain special characters.")
       ,files = def &= args &= typ "Files"
     } &= program "ls" &= summary "ls: list contents of directory" &= versionArg [ignore]
  return EmptyStatus


-- Pretty print the Stat structure.

prettyPrint :: LsArgs -> [Stat] -> String

prettyPrint lsa = render . prStats lsa

prStats lsa = vcat . map (prStat lsa)

prStat lsa st = sizek <+> ulmod <+> fqid <+> tempf <+> longinfo <+> fpath <+> slash
  where 
    opt zz xx = if zz lsa then xx else empty
    qtp = qid_typ $ st_qid st
    xbits = (c_DMEXEC `shiftL` oShift) .|.
            (c_DMEXEC `shiftL` gShift) .|.
            (c_DMEXEC `shiftL` wShift)
    hex x = text (showHex x "")
    sizek = opt s $ int $ fromIntegral (st_length st `div` 1024)
    ulmod = opt m $ lbrack <> text (st_muid st) <> rbrack
    fqid = opt q $ lparen <> (hex $ qid_path $ st_qid st) <+>
                             (hex $ qid_vers $ st_qid st) <+>
                             (hex $ qtp) <> rparen
    tempf = opt oT $ if qtp .&. c_QTTMP /= 0 then text "t" 
                                             else text "-"
    longinfo = empty -- !!!!!!!!!!NB!!!!!!!!!
    fpath = text (st_name st)
    slash = opt oF $ if qtp .&. c_QTDIR /= 0
                       then text "/"
                       else if st_mode st .&. xbits /= 0 then text "*"
                                                         else empty
    
