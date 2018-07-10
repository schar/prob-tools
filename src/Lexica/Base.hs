{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.Base where

import Lexica
import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- second lexicon: Base carries terms to familiar e/s/t denotations
------------------------------------------------------------------------------

data Base a = B {runBase :: (TypeOf a)}

instance Grammar Base where
  s (B x) (B f)     = B $ \w -> x `elem` f w
  tvp (B f) (B x)   = B $ \w -> [y | (x,y) <- f w]
  nil               = B $ const True

instance Eval Base where
  eval              = runBase

instance NameLex Base where
  john              = B $ John
  mary              = B $ Mary

instance SALex Base where
  scored            = B $ \w -> nub [x | y <- shot' w, (x,y) <- hit' w]
  aced              = B $ \w -> nub [x | (x,_) <- hit' w, shot' w == shot' w `intersect` [y | (z,y) <- hit' w, z==x]]

instance GQLex Base where
  johnQ q           = q john
  maryQ q           = q mary
  noPlayer q        = B $ \w -> not (any (\y -> eval (q (B y)) w) (player' w))
  somePlayer q      = B $ \w -> any (\y -> eval (q (B y)) w) (player' w)
  everyPlayer q     = B $ \w -> all (\y -> eval (q (B y)) w) (player' w)

instance MannerLex Base where
  started           = B $ const True
  gotStarted        = B $ const True

baseGQLex :: Lexicon GQMessage
baseGQLex = Lexicon "Base" (\(GQMessage m) -> runBase m)

baseSALex :: Lexicon SAMessage
baseSALex = Lexicon "Base" (\(SAMessage m) -> runBase m)

baseMannerLex :: Lexicon MannerMessage
baseMannerLex = Lexicon "Base" (\(MannerMessage m) -> runBase m)
