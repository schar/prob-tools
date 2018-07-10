{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.AdjGT where

import Lexica
import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- The AdjGT lexicon interprets terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------

data AdjGT a = AdjGT {runAdjGT :: (DTypeOf a)}

instance Grammar AdjGT where
  s (AdjGT x) (AdjGT f)   = AdjGT $ \d w -> x d `elem` f d w
  tvp (AdjGT f) (AdjGT x) = AdjGT $ \d w -> [y | (x,y) <- f d w]
  nil                 = AdjGT $ \_ _ -> True

instance DegEval AdjGT where
  degEval (AdjGT m) = m

instance NameLex AdjGT where
  john                = AdjGT $ const John
  mary                = AdjGT $ const Mary

instance AdjLex AdjGT where
  tall = AdjGT $ \d w -> [x' | (x',d') <- height' w, d' >= d]
  short = AdjGT $ \d w -> [x' | (x',d') <- height' w, d' < d]

baseAdjGTLex :: Lexicon AdjMessage
baseAdjGTLex = Lexicon "baseAdjGTLex" (\(AdjMessage m) -> runAdjGT m 0.5)
