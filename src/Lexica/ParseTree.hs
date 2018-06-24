{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.ParseTree where

import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)

-- a lexicon is an algebra that evaluates object language terms
------------------------------------------------------------------------------

