------------------------------------------------------------------
-- |
-- Module      :  Data.Plastic.State
-- Copyright   :  (c) Dmitry Golubovsky, 2011
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Mutable state necessary for proper running Plastic code
------------------------------------------------------------------

module Data.Plastic.State (
  PlasticT
 ,runPlasticT
) where

import Data.Plastic.SymbolTrie
import Data.Plastic.GraphPrim
import Data.Plastic.Step
import Control.Monad.Trans.RWS.Strict
import qualified Data.IntMap as I

-- The state component of Plastic consists of the following three layers:
--  - R: an IntMap containing the system dictionary 
--       a Trie containing the symbols map
--  - W: a list of graphics primitives which is filled only when the drawing operation
--       is performed
--  - S: an Int counter to generate unique numbers for the system dictionary entries
--       an IntMap containing the recent changes to the system dictionary
--       a Trie containing the recent changes to the symbols map
       
data PlEnv = PlEnv {
  symmap :: SymbolTrie
 ,sysdict :: I.IntMap Value
} deriving (Show)

data PlState = PlState {
  syscnt :: Int
 ,chgmap :: SymbolTrie
 ,chgdict :: I.IntMap Value
} deriving (Show)

type PlasticT m a = RWST PlEnv [GraphPrim] PlState m a

runPlasticT :: PlasticT m a -> PlEnv -> PlState -> m (a, PlState, [GraphPrim])

runPlasticT = runRWST



