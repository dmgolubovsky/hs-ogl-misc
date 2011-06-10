------------------------------------------------------------------
-- |
-- Module      :  Data.Plastic.SymbolTrie
-- Copyright   :  (c) Dmitry Golubovsky, 2011
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A trie-based map to store the symbol lookup-by-name table
------------------------------------------------------------------


module Data.Plastic.SymbolTrie (
  Symbol (..)
 ,mkSymTrie
 ,mkSymbol
) where

import Control.Monad
import qualified Data.Trie as T
import Data.ByteString.UTF8

-- | In Plastic, all global objects are addressed via IntMaps where Int
-- values serve as keys for faster lookups. It is however necessary to
-- be able to find symbol's numeric value by its character representation.
-- This module provides facilities to perform such lookup using a Trie
-- as a map from (byte)strings to symbols.

newtype Symbol = Symbol Int

-- | Build an empty Trie for symbols lookup by character name.

mkSymTrie :: T.Trie Symbol

mkSymTrie = T.empty

-- | Given a character name, find or create a symbol for it. This function is expected
-- to operate in a Monadic environment providing some sort of state, with the Symbol Trie
-- being part of that state. Two functions: one to generate an unique Int value (to use
-- as Symbol's numeric key), another to update the Monadic state with new Trie should be
-- provided.

mkSymbol :: (Monad m)
         => (String -> m Int)                   -- ^ function to generate an unique Int value:
                                                --   may compute a hash, or call a random numbers
                                                --   generator, or return an incrementing value
         -> (T.Trie Symbol -> m ())             -- ^ function to update the Monadic state with
                                                --   modified Trie if needed
         -> T.Trie Symbol                       -- ^ the Trie to operate on
         -> String                              -- ^ character name of the symbol
         -> m Symbol                            -- ^ symbol for the string

mkSymbol genf updf trs str = do 
  let bs = fromString str
  case T.lookup bs trs of
    Just sym -> return sym
    Nothing -> do
      nsym <- genf str >>= return . Symbol
      let trs' = T.insert bs nsym trs
      updf trs'
      return nsym

      
  
  
