{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lexica.Adj
-- import Lexica.SA
-- import Lexica.GQ
-- import Lexica.Neo
import Lexica.Base
import LUM
import Prob
import Model
import Lexica
import Vocab
import Language.Haskell.TH


{--

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


-- > L0
-- > ----------
-- > P(.|John scored, baseLex): wA = 0.5, wS = 0.5
-- > P(.|John aced, baseLex): wA = 1.0
-- > P(.|Silence, baseLex): wA = 0.33, wS = 0.33, wN = 0.33

-- > S0
-- > ----------
-- > P(.|wA, baseLex): John scored = 0.33, John aced = 0.67, Silence = 0.0
-- > P(.|wS, baseLex): John scored = 1.0, Silence = 0.0
-- > P(.|wN, baseLex): Silence = 1.0

-- > L1
-- > ----------
-- > P(.|John scored): wA = 0.29, wS = 0.71
-- > P(.|John aced): wA = 1.0
-- > P(.|Silence): wA = 0.0, wS = 0.25, wN = 0.75

--}


{--}

-- stage the types and priors for LUM over Adj alternatives
------------------------------------------------------------------------------

baselex = baseAdjLex
universe = adjUniv

-- define the SA lexica that compete with Base
adjLexes :: [Lexicon AdjMessage]
adjLexes =
  [ Lexicon ("AdjLex" ++ show d) (\(AdjMessage m) w -> runAdj m d w) | d <- heights ]

-- specify the alternative utterances
messages :: [AdjMessage]
messages =
  [ AdjMessage (s john tall)
  , AdjMessage (s john short)
  , AdjMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and Adj lexica
params :: Dist d => Params d AdjMessage
params = PM
  { worldPrior   = normalize 0.5 0.15 (zip universe heights)
  , messagePrior = uniform messages
  , lexiconPrior = uniform adjLexes
  , cost         = \x -> if x == AdjMessage nil then 0 else 2
  , temp         = 4
  }

-- > L0
-- > ----------
-- > P(.|John is tall, baseAdjLex): w5 = 0.42, w6 = 0.34, w7 = 0.17, w8 = 0.06, w9 = 0.01, w10 = 0.00
-- > P(.|John is short, baseAdjLex): w0 = 0.00, w1 = 0.02, w2 = 0.10, w3 = 0.30, w4 = 0.58
-- > P(.|Silence, baseAdjLex): w0 = 0.00, w1 = 0.01, w2 = 0.04, w3 = 0.11, w4 = 0.21, w5 = 0.27, w6 = 0.21, w7 = 0.11, w8 = 0.04, w9 = 0.01, w10 = 0.00

-- > S0
-- > ----------
-- > P(.|w0, baseAdjLex): John is short = 0.02, Silence = 0.98
-- > P(.|w1, baseAdjLex): John is short = 0.02, Silence = 0.98
-- > P(.|w2, baseAdjLex): John is short = 0.02, Silence = 0.98
-- > P(.|w3, baseAdjLex): John is short = 0.02, Silence = 0.98
-- > P(.|w4, baseAdjLex): John is short = 0.02, Silence = 0.98
-- > P(.|w5, baseAdjLex): John is tall = 0.00, Silence = 1.00
-- > P(.|w6, baseAdjLex): John is tall = 0.00, Silence = 1.00
-- > P(.|w7, baseAdjLex): John is tall = 0.00, Silence = 1.00
-- > P(.|w8, baseAdjLex): John is tall = 0.00, Silence = 1.00
-- > P(.|w9, baseAdjLex): John is tall = 0.00, Silence = 1.00
-- > P(.|w10, baseAdjLex): John is tall = 0.00, Silence = 1.00

-- > L1
-- > ----------
-- > P(.|John is tall): w10 = 0.03, w9 = 0.15, w8 = 0.41, w7 = 0.36, w6 = 0.04, w5 = 0.01, w4 = 0.00, w3 = 0.00, w2 = 0.00, w1 = 0.00, w0 = 0.00
-- > P(.|John is short): w9 = 0.00, w8 = 0.00, w7 = 0.00, w6 = 0.00, w5 = 0.01, w4 = 0.04, w3 = 0.36, w2 = 0.41, w1 = 0.15, w0 = 0.03
-- > P(.|Silence): w10 = 0.00, w9 = 0.01, w8 = 0.03, w7 = 0.11, w6 = 0.22, w5 = 0.27, w4 = 0.22, w3 = 0.11, w2 = 0.03, w1 = 0.01, w0 = 0.00

--}


{--

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
params :: Dist d => Params d GQMessage
params = PM
  { worldPrior   = uniform universe
  , messagePrior = uniform messages
  , lexiconPrior = uniform neoLexes
  , cost         = \x -> if x == GQMessage nil then 5 else 0
  , temp         = 1
  }

-- > L0
-- > ----------
-- > P(.|John scored, baseLex): wSN = 0.17, wSS = 0.17, wSA = 0.17, wAN = 0.17, wAS = 0.17, wAA = 0.17
-- > P(.|John aced, baseLex): wAN = 0.33, wAS = 0.33, wAA = 0.33
-- > P(.|Mary scored, baseLex): wNS = 0.17, wNA = 0.17, wSS = 0.17, wSA = 0.17, wAS = 0.17, wAA = 0.17
-- > P(.|Mary aced, baseLex): wNA = 0.33, wSA = 0.33, wAA = 0.33
-- > P(.|some player scored, baseLex): wNS = 0.12, wNA = 0.12, wSN = 0.12, wSS = 0.12, wSA = 0.12, wAN = 0.12, wAS = 0.12, wAA = 0.12
-- > P(.|some player aced, baseLex): wNA = 0.2, wSA = 0.2, wAN = 0.2, wAS = 0.2, wAA = 0.2
-- > P(.|every player scored, baseLex): wSS = 0.25, wSA = 0.25, wAS = 0.25, wAA = 0.25
-- > P(.|every player aced, baseLex): wAA = 1.0
-- > P(.|no player scored, baseLex): wNN = 1.0
-- > P(.|no player aced, baseLex): wNN = 0.25, wNS = 0.25, wSN = 0.25, wSS = 0.25
-- > P(.|Silence, baseLex): wNN = 0.11, wNS = 0.11, wNA = 0.11, wSN = 0.11, wSS = 0.11, wSA = 0.11, wAN = 0.11, wAS = 0.11, wAA = 0.11

-- > S0
-- > ----------
-- > P(.|wNN, baseLex): no player scored = 0.8, no player aced = 0.2, Silence = 0.0
-- > P(.|wNS, baseLex): Mary scored = 0.31, some player scored = 0.23, no player aced = 0.46, Silence = 0.0
-- > P(.|wNA, baseLex): Mary scored = 0.2, Mary aced = 0.4, some player scored = 0.15, some player aced = 0.24, Silence = 0.0
-- > P(.|wSN, baseLex): John scored = 0.31, some player scored = 0.23, no player aced = 0.46, Silence = 0.0
-- > P(.|wSS, baseLex): John scored = 0.17, Mary scored = 0.17, some player scored = 0.13, every player scored = 0.26, no player aced = 0.26, Silence = 0.0
-- > P(.|wSA, baseLex): John scored = 0.13, Mary scored = 0.13, Mary aced = 0.27, some player scored = 0.1, some player aced = 0.16, every player scored = 0.2, Silence = 0.0
-- > P(.|wAN, baseLex): John scored = 0.2, John aced = 0.4, some player scored = 0.15, some player aced = 0.24, Silence = 0.0
-- > P(.|wAS, baseLex): John scored = 0.13, John aced = 0.27, Mary scored = 0.13, some player scored = 0.1, some player aced = 0.16, every player scored = 0.2, Silence = 0.0
-- > P(.|wAA, baseLex): John scored = 6.0e-2, John aced = 0.13, Mary scored = 6.0e-2, Mary aced = 0.13, some player scored = 5.0e-2, some player aced = 8.0e-2, every player scored = 0.1, every player aced = 0.39, Silence = 0.0

-- > L1
-- > ----------
-- > P(.|John scored): wSN = 0.47, wSS = 0.11, wSA = 0.21, wAN = 0.15, wAS = 4.0e-2, wAA = 2.0e-2
-- > P(.|John aced): wAN = 0.49, wAS = 0.41, wAA = 0.1
-- > P(.|Mary scored): wNS = 0.47, wNA = 0.15, wSS = 0.11, wSA = 4.0e-2, wAS = 0.21, wAA = 2.0e-2
-- > P(.|Mary aced): wNA = 0.49, wSA = 0.41, wAA = 0.1
-- > P(.|some player scored): wNS = 0.26, wNA = 8.0e-2, wSN = 0.26, wSS = 7.0e-2, wSA = 0.12, wAN = 8.0e-2, wAS = 0.12, wAA = 2.0e-2
-- > P(.|some player aced): wNA = 0.26, wSA = 0.21, wAN = 0.26, wAS = 0.21, wAA = 6.0e-2
-- > P(.|every player scored): wSS = 0.62, wSA = 0.14, wAS = 0.14, wAA = 9.0e-2
-- > P(.|every player aced): wAA = 1.0
-- > P(.|no player scored): wNN = 0.64, wNA = 0.14, wAN = 0.14, wAA = 8.0e-2
-- > P(.|no player aced): wNN = 0.28, wNS = 0.25, wSN = 0.25, wSS = 0.21
-- > P(.|Silence): wNN = 0.15, wNS = 0.14, wNA = 0.11, wSN = 0.14, wSS = 0.11, wSA = 9.0e-2, wAN = 0.11, wAS = 9.0e-2, wAA = 6.0e-2


--}


-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

-- literal listener with lex
-- print the distribution over worlds for each possible message
dispL0 :: (Eq m, Show m) => Lexicon m -> Params BDDist m -> [m] -> IO ()
dispL0 lex ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m ++ ", " ++ show lex) (l0 m lex ps) | m <- ms]

-- literal speaker with lex
-- print the distribution over messages for each possible world
dispS0 :: (Eq b, Show b) => Lexicon b -> Params BDDist b -> [World] -> IO ()
dispS0 lex ps ws = sequence_ (map putStrLn test)
  where test = [prettyDist (show w ++ ", " ++ show lex) (s0 w lex ps) | w <- ws]

-- pragmatic listener summing over lexes
-- print the distribution over worlds (summing over lexica) for each possible message
dispL1 :: (Eq m, Show m) => Params BDDist m -> [m] -> IO ()
dispL1 ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m) (l1 m ps) | m <- ms]

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispL0 baselex params messages

  putStrLn ""
  putStrLn "S0"
  putStrLn "----------"
  dispS0 baselex params universe

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispL1 params messages
