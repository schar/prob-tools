{-# LANGUAGE DeriveLift #-}

module Experiments.Manner.SimpleCost.Domain where

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

-- the Manner model
------------------------------------------------------------------------------

wNormal, wWeird :: World
[wNormal, wWeird] = [baseWorld {weird' = False}, baseWorld {weird' = True}]

instance Show World where
  show w | w == wNormal  = "wNormal"
         | w == wWeird  = "wWeird"

mannerUniv :: [World]
mannerUniv = [wNormal, wWeird]
