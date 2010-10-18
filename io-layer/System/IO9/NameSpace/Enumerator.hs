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
   nsIterText
  ,nsEnumText
  ,liftIter
  ,dbgChunks
) where

import Data.Word
import Data.Bits
import System.IO
import Data.NineP.Bits
import System.IO.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import System.IO9.DevLayer
import Data.Enumerator
import qualified Control.Exception as X
import qualified Control.Monad.CatchIO as C
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Open a 'T.Text' 'Iteratee' for the given path handle.
  
nsIterText :: (MonadIO m)
           => PathHandle                  -- ^ Handle to iterate over
           -> Word8                       -- ^ Open mode (only c_OTRUNC is meaningful)
           -> NameSpaceT m (Iteratee T.Text (NameSpaceT m) ())

nsIterText ph om = do
  h <- NameSpaceT $ liftIO $ do
    hh <- devOpen (phAttach ph) (c_OWRITE .|. (om .&. c_OTRUNC))
    hSetBuffering hh NoBuffering
    return hh
  return $ liftIter $ ET.iterHandle h

-- Lift an iteratee to NameSpaceT.

liftIter :: (MonadIO m) => Iteratee a m b -> Iteratee a (NameSpaceT m) b

liftIter iter = Iteratee $ do
	step <- NameSpaceT $ lift $ runIteratee iter
	return $ case step of
		Yield x cs -> Yield x cs
		Error err -> Error err
		Continue k -> Continue (liftIter . k)

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

-- |Print chunks as they're received from the enumerator, optionally
-- printing empty chunks.

dbgChunks :: (MonadIO m, Show a) => Bool -> Iteratee a (NameSpaceT m) ()

dbgChunks = liftIter . printChunks

