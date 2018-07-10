{-# LANGUAGE DeriveLift #-}

module Experiments.Vagueness.SimpleThreshold.Domain where

import Language.Haskell.TH.Syntax (Lift)

-- model building blocks
------------------------------------------------------------------------------
data Entity = John
            | Mary
            | ShotA
            | ShotB
            | ShotC
            deriving (Eq, Show, Ord, Enum, Bounded, Lift)

type Deg = Double

data World = W
  { hit'    :: [(Entity, Entity)]
  , shot'   :: [Entity]
  , player' :: [Entity]
  , height' :: [(Entity, Deg)]
  , weird'  :: Bool
  }
  deriving (Eq, Lift)

baseWorld :: World
baseWorld = W
  { hit' = []
  , shot' = []
  , player' = []
  , height' = []
  , weird' = False
  }

-- the Vagueness model
------------------------------------------------------------------------------
heights :: [Deg]
heights = [n / 10.0 | n <- [0..10]]

w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10 :: World
[w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10] =
  [ baseWorld {height' = [(John, d), (Mary, d)]} | d <- heights]

instance Show World where
  show w | w == w0   = "w0"
         | w == w1   = "w1"
         | w == w2   = "w2"
         | w == w3   = "w3"
         | w == w4   = "w4"
         | w == w5   = "w5"
         | w == w6   = "w6"
         | w == w7   = "w7"
         | w == w8   = "w8"
         | w == w9   = "w9"
         | w == w10  = "w10"

adjDom :: [Entity]
adjDom = [John, Mary]

adjUniv :: [World]
adjUniv = [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]
