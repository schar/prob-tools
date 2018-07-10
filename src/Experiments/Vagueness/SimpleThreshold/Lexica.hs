{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Experiments.Vagueness.SimpleThreshold.Lexica where

import Vocab
import Experiments.Vagueness.SimpleThreshold.Domain

-- Lexica interpret terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------
type Prop = World -> Bool
type family DTypeOf a where
  DTypeOf S  = Deg -> Prop
  DTypeOf NP = Deg -> Entity
  DTypeOf VP = Deg -> World -> [Entity]
  DTypeOf TV = Deg -> World -> [(Entity,Entity)]

class DegEval f where
  degEval :: f S -> Deg -> Prop

-- a message is an unevaluated obj language term of category S
------------------------------------------------------------------------------
newtype AdjMessage = AdjMessage (forall f. (Grammar f, NameLex f, AdjLex f) => f S)
mkMessageInstances ''AdjMessage 'AdjMessage

-- a median-threshold lexicon
------------------------------------------------------------------------------
data AdjGT a = AdjGT {runAdjGT :: (DTypeOf a)}

instance Grammar AdjGT where
  s (AdjGT x) (AdjGT f)   = AdjGT $ \d w -> x d `elem` f d w
  tvp (AdjGT f) (AdjGT x) = AdjGT $ \d w -> [y | (x,y) <- f d w]
  nil                     = AdjGT $ \_ _ -> True

instance DegEval AdjGT where
  degEval (AdjGT m) = m

instance NameLex AdjGT where
  john                = AdjGT $ const John
  mary                = AdjGT $ const Mary

instance AdjLex AdjGT where
  tall  = AdjGT $ \d w -> [x' | (x',d') <- height' w, d' >= d]
  short = AdjGT $ \d w -> [x' | (x',d') <- height' w, d' < d]

baseAdjGTLex :: Lexicon AdjMessage World
baseAdjGTLex = Lexicon "baseAdjGTLex" (\(AdjMessage m) -> runAdjGT m 0.5)

-- define a range of lexica by varying the threshold
------------------------------------------------------------------------------
adjLexes :: [Lexicon AdjMessage World]
adjLexes =
  [ Lexicon ("AdjLex" ++ show d) (\(AdjMessage m) w -> runAdjGT m d w) | d <- heights ]
