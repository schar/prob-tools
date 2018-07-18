{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Vocab where

import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)
import Language.Haskell.TH
import GHC.Generics (Generic)


-- categories for the object language algebra
data NP
data VP
data S
data TV

-- Lex classes define terms for different kinds of obj language expressions
------------------------------------------------------------------------------
class Grammar f where
  s      :: f NP -> f VP -> f S
  tvp    :: f TV -> f NP -> f VP
  nil    :: f S

class NameLex f where
  john   :: f NP
  mary   :: f NP

class SALex f where
  aced   :: f VP
  scored :: f VP

class GQLex f where
  johnQ       :: (f NP -> f S) -> f S
  maryQ       :: (f NP -> f S) -> f S
  noPlayer    :: (f NP -> f S) -> f S
  somePlayer  :: (f NP -> f S) -> f S
  everyPlayer :: (f NP -> f S) -> f S

class AdjLex f where
  tall :: f VP
  short :: f VP

class MannerLex f where
  started :: f S
  gotStarted :: f S


-- convenience type differentiating and labeling lexica
------------------------------------------------------------------------------
data Lexicon m w = Lexicon
  { lexName :: String, interpret :: m -> w -> Bool }
instance Eq (Lexicon m w) where
  (Lexicon name _) == (Lexicon name' _) = name == name'
instance Ord (Lexicon m w) where
  compare (Lexicon name _) (Lexicon name' _) = compare name name'
instance Show (Lexicon m w) where
  show (Lexicon name _) = name


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

mkMessageInstances ty dat =
  [d| instance Eq $t where
        $pm == $pm' = (m :: ParseTree S) == m'
      instance Ord $t where
        compare $pm $pm' = compare (m :: ParseTree S) m'
      instance Show $t where
        show $pm = show (m :: ParseTree S)
  |]
  where t   = conT ty
        pm  = conP dat [[p|m |]]
        pm' = conP dat [[p|m'|]]
