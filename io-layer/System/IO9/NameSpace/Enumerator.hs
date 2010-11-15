------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpace.Enumerator
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- NameSpace Layer and Monad Transformer - Enumerator interfaces
------------------------------------------------------------------

module System.IO9.NameSpace.Enumerator (
   nsWithText
  ,nsWithBin
  ,nsEnumText
  ,nsEnumBin
  ,nsEnumDir
  ,liftIter
  ,nestText
  ,nestLines
  ,dbgChunks
) where

import Data.Word
import Data.Bits
import Data.Char
import System.IO
import Data.NineP
import Data.NineP.Bits
import System.IO.Error
import System.IO9.Error
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Util
import System.IO9.NameSpace.Types
import System.IO9.DevLayer
import Data.Enumerator hiding (map)
import Data.Nesteratee
import Data.List.Split
import qualified Control.Exception as X
import qualified Control.Monad.CatchIO as C
import qualified Data.ByteString as B
import qualified Data.Enumerator.IO as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E

-- | Open an 'Iteratee' on a binary file for the given path handle, use it, and close the handle
-- afterwards.
  
nsWithBin  :: (MonadIO m, C.MonadCatchIO m)
           => PathHandle                  -- ^ Handle to iterate over
           -> Word8                       -- ^ Open mode (only c_OTRUNC is meaningful)
           -> (Iteratee B.ByteString (NameSpaceT m) () -> NameSpaceT m x)
           -> NameSpaceT m x

nsWithBin  ph om k = do
  h <- NameSpaceT $ liftIO $ do
    hh <- devOpen (phAttach ph) (c_OWRITE .|. (om .&. c_OTRUNC))
    hSetBuffering hh NoBuffering
    hSetBinaryMode hh True
    return hh
  let eit = liftIter $ EB.iterHandle h
  k eit `nsFinally` (NameSpaceT $ liftIO $ hClose h)

-- | Open a 'T.Text' 'Iteratee' for the given path handle, use it, and close the handle
-- afterwards.
  
nsWithText :: (MonadIO m, C.MonadCatchIO m)
           => PathHandle                  -- ^ Handle to iterate over
           -> Word8                       -- ^ Open mode (only c_OTRUNC is meaningful)
           -> (Iteratee T.Text (NameSpaceT m) () -> NameSpaceT m x)
           -> NameSpaceT m x

nsWithText ph om k = do
  h <- NameSpaceT $ liftIO $ do
    hh <- devOpen (phAttach ph) (c_OWRITE .|. (om .&. c_OTRUNC))
    hSetBuffering hh NoBuffering
    return hh
  let eit = liftIter $ ET.iterHandle h
  k eit `nsFinally` (NameSpaceT $ liftIO $ hClose h)

-- Lift an iteratee to NameSpaceT.

liftIter :: (MonadIO m) => Iteratee a m b -> Iteratee a (NameSpaceT m) b

liftIter iter = Iteratee $ do
	step <- NameSpaceT $ lift $ runIteratee iter
	return $ case step of
		Yield x cs -> Yield x cs
		Error err -> Error err
		Continue k -> Continue (liftIter . k)

-- | Open a binary 'Enumerator' for the given path handle.

nsEnumBin  :: (MonadIO m, C.MonadCatchIO m)
           => Integer                     -- ^ Buffer size
           -> PathHandle                  -- ^ Handle to enumerate
           -> Enumerator B.ByteString (NameSpaceT m) b

nsEnumBin  n ph s =
  Iteratee io where
    withHandle = tryStep (devOpen (phAttach ph) c_OREAD)
    io = withHandle $ \h -> do
      NameSpaceT $ liftIO $ do
        hSetBuffering h NoBuffering
        hSetBinaryMode h True
      runIteratee (enh h s) `nsFinally` (NameSpaceT $ liftIO $ hClose h)
    enh h = Iteratee . loop where
      loop (Continue k) = withBytes $ \b -> if B.null b
        then return $ Continue k
        else runIteratee (k (Chunks [b])) >>= loop
      loop step = return step
      withBytes = tryStep $ do
        hasInput <- X.catch
          (hWaitForInput h (-1))
          (\err -> if isEOFError err
            then return False
            else X.throwIO err)
        if hasInput
          then B.hGetNonBlocking h (fromIntegral n)
          else return B.empty

-- | Open a 'T.Text' 'Enumerator' for the given path handle.

nsEnumText :: (MonadIO m, C.MonadCatchIO m)
           => PathHandle                  -- ^ Handle to enumerate
           -> Enumerator T.Text (NameSpaceT m) b

nsEnumText ph s =
  Iteratee io where
    withHandle = tryStep (devOpen (phAttach ph) c_OREAD)
    io = withHandle $ \h -> runIteratee (enh h s) `nsFinally` (NameSpaceT $ liftIO $ hClose h)
    enh h = Iteratee . loop where
      loop (Continue k) = withText $ \maybeText -> case maybeText of
        Nothing -> return $ Continue k
        Just text -> runIteratee (k (Chunks [text])) >>= loop
      loop step = return step
      withText = tryStep $ X.catch
        (Just `fmap` T.hGetLine h)
        (\err -> if isEOFError err
          then return Nothing
          else X.throwIO err)

tryStep :: MonadIO m 
        => IO t 
        -> (t -> NameSpaceT m (Step a (NameSpaceT m) b)) 
        -> NameSpaceT m (Step a (NameSpaceT m) b)
tryStep get io = do
	tried <- NameSpaceT $ liftIO (X.try get)
	case tried of
		Right t -> io t
		Left err -> return $ Error err

-- | Enumerate a unioned directory. This enumerator expects an iteratee capable
-- of receiving a stream of 'Stat' structures.

nsEnumDir :: (MonadIO m, C.MonadCatchIO m)
          => PathHandle                   -- ^ Handle of a directory to enumerate
          -> Enumerator Stat (NameSpaceT m) b

nsEnumDir (PathHandle (DevAttach {devqid = q}) _) s | qid_typ q .&. c_QTDIR == 0 =
  throwError Enotdir

nsEnumDir ph s = Iteratee $ do
  (u, bds) <- NameSpaceT $ do
    ns <- asks nspace >>= liftIO . readMVar
    u <- asks hown
    let fu = findunion (phCanon ph) ns
        phs = case fu of
          [] -> [ph]
          _  -> map dirph fu
    return (u, phs)
  ss <- forM bds $ \p -> (NameSpaceT $ liftIO $ do
      let pda = phAttach p
      h <- devOpen pda c_OREAD
      fns <- hGetContents h >>= return . wordsBy (== '\000')
      forM fns $ \f -> devWalk pda f >>= devStat) `nsCatch` (\_ -> return [])
  let mss = map (mapUser u) $ concat ss
  loop mss s where
    loop [] (Continue k) = return (Continue k)
    loop (l:ls) (Continue k) = runIteratee (k (Chunks [l])) >>= loop ls
    loop l z = return z
        

-- | Print chunks as they're received from the enumerator, optionally
-- printing empty chunks.

dbgChunks :: (MonadIO m, Show a) => Bool -> Iteratee a (NameSpaceT m) ()

dbgChunks = liftIter . printChunks

-- | A 'Nesteratee' reading 'ByteString' chunks from its input stream and feeding
-- the nested 'Iteratee' with chunks of 'T.Text'. Invalid octets result in the 0xFFFD
-- character. This happens when undecoded remainder is longer than 5 octets.

nestText :: (Monad m) => Nesteratee T.Text B.ByteString m b

nestText = nestState $ \i -> let (t, b) = E.decodeUtf8Part i
                             in  if B.length b > 5 then (T.singleton (chr 0xFFFD), B.tail b)
                                                   else (t, b)

-- A 'Nesteratee' reading 'Text' chunks from its input stream and feeding the nested
-- 'Nesteratee' with lines of text (that is, delimited with a newline character).

nestLines :: (Monad m) => Nesteratee T.Text T.Text m b

nestLines = nestEOF [T.singleton '\n'] . nestState splitNL

splitNL t = let (p, r) = T.spanBy (/= '\n') t
                (p', r') = T.spanBy (== '\n') r
            in  case T.null r of
                  True ->  (T.empty, p)
                  False -> (p `T.append` p', r')


