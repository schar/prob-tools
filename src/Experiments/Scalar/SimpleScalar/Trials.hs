{-# LANGUAGE TemplateHaskell #-}

module Experiments.Scalar.SimpleScalar.Trials where

import Experiments.Scalar.SimpleScalar.Domain
import Experiments.Scalar.SimpleScalar.Lexica
-- import Lexica.Base
import LUM
import Prob
import Vocab
import Experiments

{--}

-- stage the types and priors for LUM over SA alternatives
------------------------------------------------------------------------------

-- define the SA lexica that compete with Base
$(mkSALexes)
-- from Lexica.SA: saLexes = [\m -> eval (m :: l S) | l <- refineBase ...]

baselex = baseSALex
universe = saUniv

-- specify the alternative utterances
messages :: [SAMessage]
messages =
  [ SAMessage (s john scored)
  , SAMessage (s john aced)
  , SAMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and SA lexica
params :: Dist d => Params d SAMessage
params = PM
  { worldPrior   = uniform universe
  , messagePrior = uniform messages
  , lexiconPrior = uniform saLexes
  , cost         = \x -> if x == SAMessage nil then 5 else 0
  , temp         = 1
  }


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
-- > P(.|John scored, Base): wA = 0.50, wS = 0.50
-- > P(.|John aced, Base): wA = 1.00
-- > P(.|----, Base): wA = 0.33, wS = 0.33, wN = 0.33

-- > S0
-- > ----------
-- > P(.|wA, Base): John scored = 0.33, John aced = 0.67, ---- = 0.00
-- > P(.|wS, Base): John scored = 1.00, ---- = 0.00
-- > P(.|wN, Base): ---- = 1.00

-- > L1
-- > ----------
-- > P(.|John scored): wA = 0.29, wS = 0.71
-- > P(.|John aced): wA = 1.00
-- > P(.|----): wA = 0.00, wS = 0.25, wN = 0.75

--}
