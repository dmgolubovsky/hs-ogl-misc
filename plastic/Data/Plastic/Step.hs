------------------------------------------------------------------
-- |
-- Module      :  Data.Plastic.Step
-- Copyright   :  (c) Dmitry Golubovsky, 2011
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Stepping interpreter of Plastic threaded code
------------------------------------------------------------------

module Data.Plastic.Step (
  Proto (..)
 ,Step (..)
 ,Frame (..)
 ,Value (..)
 ,Local (..)
 ,dis
) where

import Data.List
import Data.Plastic.SymbolTrie
import qualified Data.IntMap as I

-- | The prototype of all Plastic objects. It contains an IntMap (dictionary) which is keyed
-- by integers corresponding to symbols. Each entry of the map are threaded codes:
-- even value fields are represented with proper Step variants.

data Proto = Proto {
  pdict :: I.IntMap Step
 ,pclass :: Symbol
 ,psuper :: Symbol
}

instance Show Proto where
  show (Proto d c s) = "<proto>"

-- | Current frame to hold local variables. Unlike the proto dictionary, it only
-- holds values. Local variables are also keyed by integer numbers.

newtype Frame = Frame (I.IntMap Value)

-- | Values used in Plastic. They represent symbols, proto-objects, and primitive
-- types.

data Value = 
   ValProto Proto                              -- ^ proto-object for private reference
 | ValRef Symbol                               -- ^ global reference via the system dictionary
 | ValLoc Local                                -- ^ local variable retrieval
 | ValInt Integer                              -- ^ a big integer value
 | ValSmall Int                                -- ^ a small integer
 | ValDouble Double                            -- ^ a double precision value
 | ValFloat Float                              -- ^ a float value
 | ValList [Value]                             -- ^ a list of Values
 | ValString String                            -- ^ a string value (not a list of characters)
 | Self                                        -- ^ use reference to the current object context
 | It                                          -- ^ use the value passed by the previous step
 | Nil                                         -- ^ no value
  deriving (Show)

-- | Local variables are indeed integer indices in the current frame IntMap.

newtype Local = Local Int

instance Show Local where
  show (Local l) = 't' : (show l)

-- | The scripts driving the Paper/Plastic GUI are encoded as threaded (continuated)
-- data structures. Each step of the threaded code has pointer to the next step (except
-- for the very last one).

data Step = 
   Send Value Symbol [Value] Step              -- ^ send a message to the target obtained from
                                               --   the previous step
                                               --   the last member is continuation step
 | Store Value Local Step                      -- ^ store the result of previous step in a local
                                               --   variable
 | Return Value                                -- ^ pass the result of the previous step
                                               --   to the linked continuation step
  deriving (Show)

class (Show a) => Disasm a where
  dis :: a -> String
  dis a = show a

instance Disasm Local

instance Disasm Symbol

dissv s vs = 
  let (ps, ls) = symParts s
  in  concat $ intersperse " " $ zipWith (++) ps (take ls $ map dis vs)

disch ss s@(Send It _ _ k) = disch (ss ++ [s]) k
disch ss (Store It loc k) = dis loc ++ " := (" ++ prtchain ss ++ ").\n" ++ dis k
disch ss (Return It) = "^ (" ++ prtchain ss ++ ").\n"

prtchain [] = "Nil"
prtchain ((Send v s vs k):ss) = dis v ++ " " ++ dissv s vs ++ prtc ss where
  prtc [] = ""
  prtc ((Send v s vs k):ss) = "; " ++ dissv s vs ++ prtc ss

chained (Send It _ _ _) = True
chained (Store It _ _) = True
chained (Return It) = True
chained _ = False

instance Disasm Value where
  dis (ValLoc t) = show t
  dis (ValInt i) = show i
  dis (ValFloat f) = show f
  dis (ValSmall i) = show i
  dis (ValDouble d) = show d
  dis x = show x

instance Disasm Step where
  dis (Return x) = "^ " ++ dis x
  dis st@(Send v s vs k) | chained k = disch [st] k
  dis (Send v s vs k) = dis v ++ " " ++ dissv s vs ++ ".\n" ++ dis k
  dis (Store v l k) = dis v ++ " -> " ++ dis l ++ ".\n" ++ dis k

