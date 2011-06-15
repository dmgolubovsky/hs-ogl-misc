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
  Symbol
 ,SymbolTrie
 ,mkSymTrie
 ,mkSymbol
 ,symParts
) where

import Control.Monad
import qualified Data.Trie as T
import Data.ByteString.UTF8 (fromString)

-- | In Plastic, all global objects are addressed via IntMaps where Int
-- values serve as keys for faster lookups. It is however necessary to
-- be able to find symbol's numeric value by its character representation.
-- This module provides facilities to perform such lookup using a Trie
-- as a map from (byte)strings to symbols.

data Symbol = Symbol String Int

instance Show Symbol where
  show (Symbol s i) = '#' : s

instance Eq Symbol where
  Symbol _ i1 == Symbol _ i2 = i1 == i2

-- | A Trie holding Symbols.

type SymbolTrie = T.Trie Symbol

-- | Build an empty Trie for symbols lookup by character name.

mkSymTrie :: SymbolTrie

mkSymTrie = T.empty

-- | Given a character name, find or create a symbol for it. This function is expected
-- to operate in a Monadic environment providing some sort of state, with the Symbol Trie
-- being part of that state. Three functions: one to generate an unique Int value (to use
-- as Symbol's numeric key), another to update the Monadic state with new Trie, third
-- to obtain the symbol map should be provided

mkSymbol :: (Monad m)
         => (String -> m Int)                   -- ^ function to generate an unique Int value:
                                                --   may compute a hash, or call a random numbers
                                                --   generator, or return an incrementing value
         -> (SymbolTrie -> m ())                -- ^ function to update the Monadic state with
                                                --   modified Trie if needed
         -> (m SymbolTrie)                      -- ^ the Trie to operate on
         -> String                              -- ^ character name of the symbol
         -> m Symbol                            -- ^ symbol for the string

mkSymbol genf updf mtrs str = do 
  let bs = fromString str
  trs <- mtrs
  case T.lookup bs trs of
    Just sym -> return sym
    Nothing -> do
      nsym <- genf str >>= return . (Symbol str)
      let trs' = T.insert bs nsym trs
      updf trs'
      return nsym


-- | Split symbol name into parts as delimited by colons, and find how many arguments
-- a message with this symbol takes.
      
symParts :: Symbol -> ([String], Int)

symParts (Symbol s _) = 
  let eqcol = (== ':')
      ps = parts eqcol s
      cs = map (:[]) (filter eqcol s)
      ls = case cs of
             [] -> 0
             _ -> length ps
  in  (zipWith (++) ps cs, ls)

  
parts pred s = case dropWhile pred s of
                                     [] -> []
                                     s' -> w : parts pred s''
                                         where (w, s'') = break pred s'
 
