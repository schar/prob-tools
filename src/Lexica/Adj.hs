{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.Adj where

import Lexica
import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- second lexicon: Adj carries terms to familiar e/s/t denotations
------------------------------------------------------------------------------

data Adj a = Adj {runAdj :: (DTypeOf a)}

instance Grammar Adj where
  s (Adj x) (Adj f)     = Adj $ \d w -> x d `elem` f d w
  tvp (Adj f) (Adj x)   = Adj $ \d w -> [y | (x,y) <- f d w]
  nil               = Adj $ \_ _ -> True

-- instance Eval Adj where
--   eval              = runAdj

instance NameLex Adj where
  john              = Adj $ const John
  mary              = Adj $ const Mary

instance SALex Adj where
  scored            = Adj $ \d w -> nub [x | y <- shot' w, (x,y) <- hit' w]
  aced              = Adj $ \d w -> nub [x | (x,_) <- hit' w, shot' w == shot' w `intersect` [y | (z,y) <- hit' w, z==x]]

instance GQLex Adj where
  johnQ q           = q john
  maryQ q           = q mary
  noPlayer q      = Adj $ \d w -> not (any (\y -> runAdj (q (Adj $ const y)) d w) (player' w))
  somePlayer q      = Adj $ \d w -> any (\y -> runAdj (q (Adj $ const y)) d w) (player' w)
  everyPlayer q     = Adj $ \d w -> all (\y -> runAdj (q (Adj $ const y)) d w) (player' w)

instance AdjLex Adj where
  tall = Adj $ \d w -> [x' | (x',d') <- height' w, d' >= d]
  short = Adj $ \d w -> [x' | (x',d') <- height' w, d' < d]

baseAdjLex :: Lexicon AdjMessage
baseAdjLex = Lexicon "baseAdjLex" (\(AdjMessage m) -> runAdj m 0.5)
