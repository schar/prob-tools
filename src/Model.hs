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
wN, wS, wA :: World
[wN, wS, wA] =
  [ W {hit' = [                          ], shot' = [ShotA,ShotB], player' = [John]}
  , W {hit' = [(John,ShotA)              ], shot' = [ShotA,ShotB], player' = [John]}
--, W {hit' = [              (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  , W {hit' = [(John,ShotA), (John,ShotB)], shot' = [ShotA,ShotB], player' = [John]}
  ]
wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA :: World
[wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA] =
  [ W {hit' = [                                                      ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [                                          (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [                            (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA)                                          ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA)              , (Mary,ShotA)              ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA)              , (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA), (John,ShotB)                            ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA), (John,ShotB), (Mary,ShotA)              ], shot' = [ShotA,ShotB], player' = [John,Mary]}
  , W {hit' = [(John,ShotA), (John,ShotB), (Mary,ShotA), (Mary,ShotB)], shot' = [ShotA,ShotB], player' = [John,Mary]}
  ]

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

saDom, gqDom :: [Entity]
saDom = [John,ShotA,ShotB]
gqDom = [John, Mary, ShotA, ShotB]

saUniv, gqUniv :: [World]
saUniv = [wA, wS, wN]
gqUniv = [wNN, wNS, wNA, wSN, wSS, wSA, wAN, wAS, wAA]
