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

type Deg = Double

data World = W
  { hit'    :: [(Entity, Entity)]
  , shot'   :: [Entity]
  , player' :: [Entity]
  , height' :: [(Entity, Deg)]
  }
  deriving (Eq, Lift)

baseWorld :: World
baseWorld = W
  { hit' = []
  , shot' = []
  , player' = []
  , height' = []
  }

-- the SA model
------------------------------------------------------------------------------
wN, wS, wA :: World
[wN, wS, wA] =
  [ baseWorld {hit' = [                          ], shot' = [ShotA,ShotB], player' = [John]}
  , baseWorld {hit' = [(John,ShotA)              ], shot' = [ShotA,ShotB], player' = [John]}
--, baseWorld {hit' = [              (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  , baseWorld {hit' = [(John,ShotA), (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  ]

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

-- the Adj model
------------------------------------------------------------------------------
heights :: [Deg]
heights = [n / 10.0 | n <- [0..10]]

w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10 :: World
[w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10] =
  [ baseWorld {height' = [(John, d), (Mary, d)]} | d <- heights]

instance Show World where
  show w | w == wA   = "wA"
         | w == wS   = "wS"
         | w == wN   = "wN"
         | w == wNN  = "wNN"
         | w == wNS  = "wNS"
         | w == wNA  = "wNA"
         | w == wSN  = "wSN"
         | w == wSS  = "wSS"
         | w == wSA  = "wSA"
         | w == wAN  = "wAN"
         | w == wAS  = "wAS"
         | w == wAA  = "wAA"
         | w == w0   = "w0"
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

saDom, gqDom :: [Entity]
saDom = [John,ShotA,ShotB]
gqDom = [John, Mary, ShotA, ShotB]
adjDom = [John, Mary]

saUniv, gqUniv, adjUniv :: [World]
saUniv = [wA, wS, wN]
gqUniv = [wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA]
adjUniv = [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]
