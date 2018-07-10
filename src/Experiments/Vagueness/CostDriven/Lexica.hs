{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Experiments.Vagueness.CostDriven.Lexica where

import Lexica
import Vocab
import Experiments.Vagueness.CostDriven.Domain
-- import Data.Tree
-- import Data.Functor.Classes (compare1)
-- import Data.List            (intercalate, nub, intersect)

-- The AdjEQ lexicon interprets terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------

type family DTypeOf a where
  DTypeOf S  = Deg -> Prop
  DTypeOf NP = Deg -> Entity
  DTypeOf VP = Deg -> World -> [Entity]
  DTypeOf TV = Deg -> World -> [(Entity,Entity)]

class DegEval f where
  degEval :: f S -> Deg -> Prop

newtype AdjMessage = AdjMessage (forall f. (Grammar f, NameLex f, AdjLex f) => f S)
instance Eq AdjMessage where
  (AdjMessage m) == (AdjMessage m') = (m :: ParseTree S) == m'
instance Ord AdjMessage where
  compare (AdjMessage m) (AdjMessage m') = compare (m :: ParseTree S) m'
instance Show AdjMessage where
  show (AdjMessage m) = show (m :: ParseTree S)

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

-- define the SA lexica that compete with Base
adjLexes :: [Lexicon AdjMessage]
adjLexes =
  [ Lexicon ("AdjLex" ++ show d) (\(AdjMessage m) w -> runAdjEQ m d w) | d <- heights ]
