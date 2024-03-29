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
import System.Locale
import System.IO9.Error
import System.FilePath
import System.IO9.NameSpaceT
import System.IO9.Application
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import System.Console.CmdArgs
import Text.PrettyPrint.Boxes
import Data.Text (pack)
import Data.Char
import Data.Bits
import Data.List
import Data.Maybe
import Data.NineP
import Data.NineP.Bits
import Data.NineP.Posix
import Data.Nesteratee
import Data.Time.Format
import Data.Time.Clock.POSIX
import Data.Enumerator (run, (==<<), enumList)
import Data.Enumerator.List (consume)

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
  appout <- nsStdOut
  nsWithText appout c_OTRUNC $ \out ->
    forM (files cas) $ \f -> do
      prt <- stats cas f >>= return . pack . prettyPrint cas . dirsort cas
      run (enumList 1024 [prt] ==<< out)
  return EmptyStatus

-- Sort the directory entries.

dirsort :: LsArgs -> [Stat] -> [Stat]

dirsort lsa ds | n lsa = ds

dirsort lsa ds | r lsa = reverse $ dirsort lsa {r = False} ds

dirsort lsa ds | t lsa = sortBy (\x y -> compare (st_mtime x) (st_mtime y)) ds

dirsort lsa ds | t lsa && u lsa = sortBy (\x y -> compare (st_atime x) (st_atime y)) ds

dirsort lsa ds = sortBy (\x y -> compare (st_name x) (st_name y)) ds 

-- Collect Stat structures for a single argument.

stats :: (MonadIO m, MonadCatchIO m) => LsArgs -> FilePath -> NameSpaceT m [Stat]

stats lsa fp = do
  ph <- nsEval fp
  st <- nsStat ph
  let isdir zs = (qid_typ $ st_qid zs) .&. c_QTDIR /= 0
  case isdir st of
    False -> return [fxn lsa fp st]
    True | d lsa -> return [fxn lsa fp st]
    True -> do
      ests <- run (nsEnumDir ph ==<< consume)
      case ests of
        Left err -> nsThrow $ Located (show ph) $ OtherError (show err)
        Right sts | p lsa -> return sts
        Right sts -> return $ map (\st -> st {st_name = fp </> st_name st}) sts
      
-- Fix the file name shown: if the p flag in lsa is false, substitute the full path
-- in the st_name field of Stat.

fxn lsa fp st | p lsa = st

fxn lsa fp st = st {st_name = fp}

-- Pretty print the Stat structure.

prettyPrint :: LsArgs -> [Stat] -> String

prettyPrint lsa = render . prStats lsa

prStats lsa sts = sizek <> ulmod <> fqid <> tempf <> longinfo <> fpath <> slash
  where 
    opt zz xx = if zz lsa then (xx <+> nullBox) else nullBox
    mkbox al fld strs = let txts = map fld strs
                            txti = map text txts
                        in  vcat al txti
    qtp = qid_typ . st_qid
    xbits = (c_DMEXEC `shiftL` oShift) .|.
            (c_DMEXEC `shiftL` gShift) .|.
            (c_DMEXEC `shiftL` wShift)
    hex n x = let s = showHex x "" in replicate (n - length s) '0' ++ s
    qempty r "" = "'" ++ r ++ "'"
    qempty _ s = s
    sizek = opt s $ mkbox right (show . (`div` 1024) . st_length) sts
    ulmod = opt m $ mkbox left (\st -> "[" ++ qempty "" (st_muid st) ++ "]") sts
    cbox al = mkbox al . const
    fqid = opt q (cbox left "(" sts <> qpath <+> qvers <+> qtyp <> cbox right ")" sts)
    qpath = mkbox left (hex 16 . qid_path . st_qid) sts
    qvers = mkbox right (show . qid_vers . st_qid) sts
    qtyp = mkbox right (hex 2 . qtp) sts
    tempf = opt oT $ mkbox left (\st -> if qtp st .&. c_QTTMP /= 0 then "t" else "-") sts
    longinfo = opt l (perms <+> dev <+> dtyp <+> uid <+> gid <+> flen <+> ftime)
    dev = mkbox left ((:[]) . chr . fromIntegral . st_typ) sts
    dtyp = mkbox right (show . st_dev) sts
    uid = mkbox left (qempty "?" . st_uid) sts
    gid = mkbox left (qempty "?" . st_gid) sts
    flen = mkbox right (show . st_length) sts
    timefld = if u lsa then st_atime else st_mtime
    fmt1 = "%b %e %G"
    fmt2 = "%b %e %R"
    ftime = mkbox right (formatTime defaultTimeLocale fmt2 . 
                                    posixSecondsToUTCTime . 
                                    realToFrac . timefld) sts
    perms = bdir <> bexcl <> bperm oShift <> bperm gShift <> bperm wShift
    bdir = mkbox left (\st -> case 0 of _ | st_mode st .&. c_DMDIR /= 0 -> "d"
                                          | st_mode st .&. c_DMAPPEND /= 0 -> "a"
                                          | otherwise -> "-") sts
    bb b s = mkbox left (\st -> if st_mode st .&. b /=0 then s else "-") sts
    bexcl = bb c_DMEXCL "l"
    bperm s = bb (c_DMREAD `shiftL` s) "r"  <>
              bb (c_DMWRITE `shiftL` s) "w" <>
              bb (c_DMEXEC `shiftL` s) "x"
    fpath = mkbox left st_name sts
    slash = opt oF $ mkbox left (\st -> if qtp st .&. c_QTDIR /= 0
                                           then "/"
                                           else if st_mode st .&. xbits /= 0 then "*"
                                                                             else " ") sts
    
