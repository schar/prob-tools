{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Experiments.Vagueness.CostDriven.Lexica where

import Vocab
import Experiments.Vagueness.CostDriven.Domain

-- The AdjEQ lexicon interprets terms as threshold-vector-dependent e/s/t denotations
------------------------------------------------------------------------------
type Prop = World -> Bool
type family DTypeOf a where
  DTypeOf S  = (Deg, Deg) -> Prop
  DTypeOf NP = (Deg, Deg) -> Entity
  DTypeOf VP = (Deg, Deg) -> World -> [Entity]
  DTypeOf TV = (Deg, Deg) -> World -> [(Entity,Entity)]

class DegEval f where
  degEval :: f S -> (Deg, Deg) -> Prop

-- a message is an unevaluated obj language term of category S
------------------------------------------------------------------------------
newtype AdjMessage = AdjMessage (forall f. (Grammar f, NameLex f, AdjLex f) => f S)
mkMessageInstances ''AdjMessage 'AdjMessage

-- a median-threshold lexicon
------------------------------------------------------------------------------
data AdjEQ a = AdjEQ {runAdjEQ :: (DTypeOf a)}

instance Grammar AdjEQ where
  s (AdjEQ x) (AdjEQ f)   = AdjEQ $ \ds w -> x ds `elem` f ds w
  tvp (AdjEQ f) (AdjEQ x) = AdjEQ $ \ds w -> [y | (x,y) <- f ds w]
  nil                     = AdjEQ $ \_ _ -> True

instance DegEval AdjEQ where
  degEval (AdjEQ m) = m

instance NameLex AdjEQ where
  john                = AdjEQ $ const John
  mary                = AdjEQ $ const Mary

instance AdjLex AdjEQ where
  tall  = AdjEQ $ \ds w -> [x' | (x',d') <- height' w, d' >= fst ds]
  short = AdjEQ $ \ds w -> [x' | (x',d') <- height' w, d' <= snd ds]

baseAdjEQLex :: Lexicon AdjMessage World
baseAdjEQLex = Lexicon "baseAdjEQLex" (\(AdjMessage m) -> runAdjEQ m (0.5, 0.5))

-- define a range of lexica by varying the threshold
------------------------------------------------------------------------------
adjLexes :: [Lexicon AdjMessage World]
adjLexes = do
  d  <- heights
  d' <- heights
  return $ Lexicon ("AdjLex" ++ show (d,d')) (\(AdjMessage m) w -> runAdjEQ m (d,d') w)
