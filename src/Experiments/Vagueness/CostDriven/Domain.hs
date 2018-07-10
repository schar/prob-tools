{-# LANGUAGE DeriveLift #-}

module Experiments.Vagueness.CostDriven.Domain where

import Language.Haskell.TH.Syntax (Lift)

-- model building blocks
------------------------------------------------------------------------------
data Entity = John
            | Mary
            deriving (Eq, Show, Ord, Enum, Bounded, Lift)

type Deg = Double

data World = W
  { height' :: [(Entity, Deg)]
  }
  deriving (Eq, Lift)

baseWorld :: World
baseWorld = W
  { height' = []
  }

-- the Vagueness model
------------------------------------------------------------------------------
heights :: [Deg]
heights = 0.05 : [n / 4.0 | n <- [1..3]] ++ [0.95]

w0, w1, w2, w3, w4 :: World
[w0, w1, w2, w3, w4] =
  [ baseWorld {height' = [(John, d), (Mary, d)]} | d <- heights]

instance Show World where
  show w | w == w0   = "w0"
         | w == w1   = "w1"
         | w == w2   = "w2"
         | w == w3   = "w3"
         | w == w4   = "w4"

adjDom :: [Entity]
adjDom = [John, Mary]

adjUniv :: [World]
adjUniv = [w0, w1, w2, w3, w4]
