{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.AdjEQ where

import Lexica
import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- The AdjEQ lexicon interprets terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------

data AdjEQ a = AdjEQ {runAdjEQ :: (DTypeOf a)}

instance Grammar AdjEQ where
  s (AdjEQ x) (AdjEQ f)   = AdjEQ $ \d w -> x d `elem` f d w
  tvp (AdjEQ f) (AdjEQ x) = AdjEQ $ \d w -> [y | (x,y) <- f d w]
  nil                 = AdjEQ $ \_ _ -> True

instance DegEval AdjEQ where
  degEval (AdjEQ m) = m

instance NameLex AdjEQ where
  john                = AdjEQ $ const John
  mary                = AdjEQ $ const Mary

instance AdjLex AdjEQ where
  tall = AdjEQ $ \d w -> [x' | (x',d') <- height' w, d' == d]
  short = AdjEQ $ \d w -> [x' | (x',d') <- height' w, d' == d]

baseAdjEQLex :: Lexicon AdjMessage
baseAdjEQLex = Lexicon "baseAdjEQLex" (\(AdjMessage m) -> runAdjEQ m 0.5)
