{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Vocab where

import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- categories for the object language algebra
data NP
data VP
data S
data TV

type Prop = World -> Bool
type family TypeOf a where
  TypeOf S  = Prop
  TypeOf NP = Entity
  TypeOf VP = World -> [Entity]
  TypeOf TV = World -> [(Entity,Entity)]

type family DTypeOf a where
  DTypeOf S  = Deg -> Prop
  DTypeOf NP = Deg -> Entity
  DTypeOf VP = Deg -> World -> [Entity]
  DTypeOf TV = Deg -> World -> [(Entity,Entity)]

-- Lex classes define terms for different kinds of obj language expressions
------------------------------------------------------------------------------
class Grammar f where
  s      :: f NP -> f VP -> f S
  tvp    :: f TV -> f NP -> f VP
  nil    :: f S

class Eval f where
  eval   :: f S -> Prop

class (Grammar f) => NameLex f where
  john   :: f NP
  mary   :: f NP

class (NameLex f) => SALex f where
  aced   :: f VP
  scored :: f VP

class (SALex f) => GQLex f where
  johnQ       :: (f NP -> f S) -> f S
  maryQ       :: (f NP -> f S) -> f S
  noPlayer    :: (f NP -> f S) -> f S
  somePlayer  :: (f NP -> f S) -> f S
  everyPlayer :: (f NP -> f S) -> f S

class (NameLex f) => AdjLex f where
  tall :: f VP
  short :: f VP

-- a message is an unevaluated obj language term of category S
------------------------------------------------------------------------------
newtype GQMessage = GQMessage (forall f. (Grammar f, Eval f, NameLex f, SALex f, GQLex f) => f S)
instance Eq GQMessage where
  (GQMessage m) == (GQMessage m') = (m :: ParseTree S) == m'
instance Ord GQMessage where
  compare (GQMessage m) (GQMessage m') = compare (m :: ParseTree S) m'
instance Show GQMessage where
  show (GQMessage m) = show (m :: ParseTree S)

newtype SAMessage = SAMessage (forall f. (Grammar f, Eval f, NameLex f, SALex f) => f S)
instance Eq SAMessage where
  (SAMessage m) == (SAMessage m') = (m :: ParseTree S) == m'
instance Ord SAMessage where
  compare (SAMessage m) (SAMessage m') = compare (m :: ParseTree S) m'
instance Show SAMessage where
  show (SAMessage m) = show (m :: ParseTree S)

newtype AdjMessage = AdjMessage (forall f. (Grammar f, NameLex f, AdjLex f) => f S)
instance Eq AdjMessage where
  (AdjMessage m) == (AdjMessage m') = (m :: ParseTree S) == m'
instance Ord AdjMessage where
  compare (AdjMessage m) (AdjMessage m') = compare (m :: ParseTree S) m'
instance Show AdjMessage where
  show (AdjMessage m) = show (m :: ParseTree S)

-- The ParseTree lexicon interprets terms as trees of strings
------------------------------------------------------------------------------
instance (Ord a) => Ord (Tree a) where
  compare = compare1
data ParseTree a = Nil | PT (Tree String) deriving (Eq, Ord)

instance Show (ParseTree a) where
  show (PT tree)    = foldTree (\x cs -> if null cs then x else intercalate " " cs) tree
  show Nil          = "----"

instance Grammar ParseTree where
  s (PT x) (PT f)   = PT $ Node "" [x,f]
  tvp (PT f) (PT x) = PT $ Node "" [f,x]
  nil               = Nil

instance Eval ParseTree where
  eval _ _          = True

instance NameLex ParseTree where
  john              = PT $ pure "John"
  mary              = PT $ pure "Mary"

instance SALex ParseTree where
  aced              = PT $ pure "aced"
  scored            = PT $ pure "scored"

instance GQLex ParseTree where
  johnQ       q    = q (PT $ Node "" [pure "John" ])
  maryQ       q    = q (PT $ Node "" [pure "Mary" ])
  noPlayer    q    = q (PT $ Node "" [pure "no player"])
  somePlayer  q    = q (PT $ Node "" [pure "some player" ])
  everyPlayer q    = q (PT $ Node "" [pure "every player"])

instance AdjLex ParseTree where
  tall             = PT $ pure "is tall"
  short            = PT $ pure "is short"
