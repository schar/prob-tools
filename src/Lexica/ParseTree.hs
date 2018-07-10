{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.ParseTree where

import Vocab
-- import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

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

-- instance Eval ParseTree where
--   eval _ _          = True

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

instance MannerLex ParseTree where
  started          = PT $ pure "started"
  gotStarted       = PT $ pure "got started"
