{-# LANGUAGE DeriveLift #-}

module Experiments.Scalar.SimpleScalar.Domain where

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
  , worldName :: String
  }
  deriving (Eq, Lift)

baseWorld :: World
baseWorld = W
  { hit' = []
  , shot' = []
  , player' = []
  , worldName = ""
  }

-- the SA model
------------------------------------------------------------------------------
wN, wS, wA :: World
[wN, wS, wA] =
  [ baseWorld {hit' = [                          ], shot' = [ShotA,ShotB], player' = [John], worldName = "wN"}
  , baseWorld {hit' = [(John,ShotA)              ], shot' = [ShotA,ShotB], player' = [John], worldName = "wS"}
--, baseWorld {hit' = [              (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  , baseWorld {hit' = [(John,ShotA), (John,ShotB)], shot' = [ShotA,ShotB], player' = [John], worldName = "wA"}
  ]

instance Show World where
  show = worldName
instance Ord World where
  compare w w' = compare (worldName w) (worldName w')

saDom :: [Entity]
saDom = [John,ShotA,ShotB]

saUniv :: [World]
saUniv = [wA, wS, wN]
