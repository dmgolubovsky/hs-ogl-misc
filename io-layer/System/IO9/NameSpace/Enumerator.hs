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
  ,liftIter
) where

import Data.Word
import Data.Bits
import System.IO
import Data.NineP.Bits
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import System.IO9.DevLayer
import qualified Control.Monad.CatchIO as C
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T


-- | Open a 'T.Text' 'E.Iteratee' for the given path handle.
  
nsIterText :: (MonadIO m)
           => PathHandle                  -- ^ Handle to iterate over
           -> Word8                       -- ^ Open mode (only c_OTRUNC is meaningful)
           -> NameSpaceT m (E.Iteratee T.Text (NameSpaceT m) ())

nsIterText ph om = do
  h <- NameSpaceT $ liftIO $ do
    hh <- devOpen (phAttach ph) (c_OWRITE .|. (om .&. c_OTRUNC))
    hSetBuffering hh NoBuffering
    return hh
  return $ liftIter $ ET.iterHandle h

-- Lift an iteratee to NameSpaceT.

liftIter :: (MonadIO m) => E.Iteratee a m b -> E.Iteratee a (NameSpaceT m) b

liftIter iter = E.Iteratee $ do
	step <- NameSpaceT $ lift $ E.runIteratee iter
	return $ case step of
		E.Yield x cs -> E.Yield x cs
		E.Error err -> E.Error err
		E.Continue k -> E.Continue (liftIter . k)

