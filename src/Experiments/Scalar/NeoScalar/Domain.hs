{-# LANGUAGE DeriveLift #-}

module Experiments.Scalar.NeoScalar.Domain where

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

-- the GQ model
------------------------------------------------------------------------------
wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA :: World
[wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA] =
  [ baseWorld {hit' = [                                                      ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [                                          (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [                            (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA)                                          ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA)              , (Mary,ShotA)              ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA)              , (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA), (John,ShotB)                            ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA), (John,ShotB), (Mary,ShotA)              ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , baseWorld {hit' = [(John,ShotA), (John,ShotB), (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  ]

instance Show World where
  show w | w == wNN  = "wNN"
         | w == wNS  = "wNS"
         | w == wNA  = "wNA"
         | w == wSN  = "wSN"
         | w == wSS  = "wSS"
         | w == wSA  = "wSA"
         | w == wAN  = "wAN"
         | w == wAS  = "wAS"
         | w == wAA  = "wAA"

saDom, gqDom :: [Entity]
saDom = [John,ShotA,ShotB]
gqDom = [John, Mary, ShotA, ShotB]
adjDom = [John, Mary]

gqUniv :: [World]
gqUniv = [wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA]
