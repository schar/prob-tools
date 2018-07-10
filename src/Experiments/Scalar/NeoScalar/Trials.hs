{-# LANGUAGE TemplateHaskell #-}

module Experiments.Scalar.NeoScalar.Trials where

import Experiments.Scalar.NeoScalar.Domain
import Experiments.Scalar.NeoScalar.Lexica
import LUM
import Prob
import Vocab


{--}

-- stage the types and priors for LUM over Neo alternatives
------------------------------------------------------------------------------

-- define the Neo lexica that compete with Base
$(mkNeoLexes)
-- from Lexica.Neo: neoLexes = [\m -> eval (m :: l S) | l <- refineBase ...]

baselex = baseGQLex
universe = gqUniv

-- specify the alternative utterances
messages :: [GQMessage]
messages =
  [ GQMessage (johnQ (\x -> s x scored))
  , GQMessage (johnQ (\x -> s x aced  ))
  , GQMessage (maryQ (\x -> s x scored))
  , GQMessage (maryQ (\x -> s x aced  ))
  , GQMessage (somePlayer (\x -> s x scored))
  , GQMessage (somePlayer (\x -> s x aced  ))
  , GQMessage (everyPlayer (\x -> s x scored))
  , GQMessage (everyPlayer (\x -> s x aced  ))
  , GQMessage (noPlayer (\x -> s x scored))
  , GQMessage (noPlayer (\x -> s x aced  ))
  , GQMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and Neo lexica
params :: Dist d => Params d GQMessage World
params = PM
  { worldPrior   = uniform universe
  , messagePrior = uniform messages
  , lexiconPrior = uniform neoLexes
  , cost         = \x -> if x == GQMessage nil then 5 else 0
  , temp         = 1
  }

-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispL0 baselex params messages

  putStrLn ""
  putStrLn "S1"
  putStrLn "----------"
  dispS1 baselex params universe

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispL1 params messages

-- > L0
-- > ----------
-- > P(.|John scored, Base): wSN = 0.17, wSS = 0.17, wSA = 0.17, wAN = 0.17, wAS = 0.17, wAA = 0.17
-- > P(.|John aced, Base): wAN = 0.33, wAS = 0.33, wAA = 0.33
-- > P(.|Mary scored, Base): wNS = 0.17, wNA = 0.17, wSS = 0.17, wSA = 0.17, wAS = 0.17, wAA = 0.17
-- > P(.|Mary aced, Base): wNA = 0.33, wSA = 0.33, wAA = 0.33
-- > P(.|some player scored, Base): wNS = 0.12, wNA = 0.12, wSN = 0.12, wSS = 0.12, wSA = 0.12, wAN = 0.12, wAS = 0.12, wAA = 0.12
-- > P(.|some player aced, Base): wNA = 0.20, wSA = 0.20, wAN = 0.20, wAS = 0.20, wAA = 0.20
-- > P(.|every player scored, Base): wSS = 0.25, wSA = 0.25, wAS = 0.25, wAA = 0.25
-- > P(.|every player aced, Base): wAA = 1.00
-- > P(.|no player scored, Base): wNN = 1.00
-- > P(.|no player aced, Base): wNN = 0.25, wNS = 0.25, wSN = 0.25, wSS = 0.25
-- > P(.|----, Base): wNN = 0.11, wNS = 0.11, wNA = 0.11, wSN = 0.11, wSS = 0.11, wSA = 0.11, wAN = 0.11, wAS = 0.11, wAA = 0.11

-- > S1
-- > ----------
-- > P(.|wNN, Base): no player scored = 0.80, no player aced = 0.20, ---- = 0.00
-- > P(.|wNS, Base): Mary scored = 0.31, some player scored = 0.23, no player aced = 0.46, ---- = 0.00
-- > P(.|wNA, Base): Mary scored = 0.20, Mary aced = 0.40, some player scored = 0.15, some player aced = 0.24, ---- = 0.00
-- > P(.|wSN, Base): John scored = 0.31, some player scored = 0.23, no player aced = 0.46, ---- = 0.00
-- > P(.|wSS, Base): John scored = 0.17, Mary scored = 0.17, some player scored = 0.13, every player scored = 0.26, no player aced = 0.26, ---- = 0.00
-- > P(.|wSA, Base): John scored = 0.13, Mary scored = 0.13, Mary aced = 0.27, some player scored = 0.10, some player aced = 0.16, every player scored = 0.20, ---- = 0.00
-- > P(.|wAN, Base): John scored = 0.20, John aced = 0.40, some player scored = 0.15, some player aced = 0.24, ---- = 0.00
-- > P(.|wAS, Base): John scored = 0.13, John aced = 0.27, Mary scored = 0.13, some player scored = 0.10, some player aced = 0.16, every player scored = 0.20, ---- = 0.00
-- > P(.|wAA, Base): John scored = 0.06, John aced = 0.13, Mary scored = 0.06, Mary aced = 0.13, some player scored = 0.05, some player aced = 0.08, every player scored = 0.10, every player aced = 0.39, ---- = 0.00

-- > L1
-- > ----------
-- > P(.|John scored): wSN = 0.47, wSS = 0.11, wSA = 0.21, wAN = 0.15, wAS = 0.04, wAA = 0.02
-- > P(.|John aced): wAN = 0.49, wAS = 0.41, wAA = 0.10
-- > P(.|Mary scored): wNS = 0.47, wNA = 0.15, wSS = 0.11, wSA = 0.04, wAS = 0.21, wAA = 0.02
-- > P(.|Mary aced): wNA = 0.49, wSA = 0.41, wAA = 0.10
-- > P(.|some player scored): wNS = 0.26, wNA = 0.08, wSN = 0.26, wSS = 0.07, wSA = 0.12, wAN = 0.08, wAS = 0.12, wAA = 0.02
-- > P(.|some player aced): wNA = 0.26, wSA = 0.21, wAN = 0.26, wAS = 0.21, wAA = 0.06
-- > P(.|every player scored): wSS = 0.62, wSA = 0.14, wAS = 0.14, wAA = 0.09
-- > P(.|every player aced): wAA = 1.00
-- > P(.|no player scored): wNN = 0.64, wNA = 0.14, wAN = 0.14, wAA = 0.08
-- > P(.|no player aced): wNN = 0.28, wNS = 0.25, wSN = 0.25, wSS = 0.21
-- > P(.|----): wNN = 0.15, wNS = 0.14, wNA = 0.11, wSN = 0.14, wSS = 0.11, wSA = 0.09, wAN = 0.11, wAS = 0.09, wAA = 0.06

--}
