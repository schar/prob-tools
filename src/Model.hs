{-# LANGUAGE DeriveLift #-}

module Model where

import Language.Haskell.TH.Syntax (Lift)

-- model building blocks
------------------------------------------------------------------------------
data Entity = John
            | Mary
            | ShotA
            | ShotB
            | ShotC
            deriving (Eq, Show, Ord, Enum, Bounded, Lift)

data World = W
  { hit'    :: [(Entity, Entity)]
  , shot'   :: [Entity]
  , player' :: [Entity]
  }
  deriving (Eq, Lift)

-- the SA model
------------------------------------------------------------------------------
wA, wS, wN :: World
[wA, wS, wN] =
  [ W {hit' = [(John,ShotA), (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  , W {hit' = [(John,ShotA)              ], shot' = [ShotA,ShotB], player' = [John]}
--, W {hit' = [              (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  , W {hit' = [                          ], shot' = [ShotA,ShotB], player' = [John]}
  ]
instance Show World where
  show w | w == wA = "wA"
         | w == wS = "wS"
         | w == wN = "wN"

saDom :: [Entity]
saDom = [John,ShotA,ShotB]

saUniv :: [World]
saUniv = [wA, wS, wN]
