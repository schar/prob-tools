{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Vocab where

-- import Model
-- import Data.Tree
-- import Data.Functor.Classes (compare1)
-- import Data.List            (intercalate, nub, intersect)

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

class (Grammar f) => MannerLex f where
  started :: f S
  gotStarted :: f S
