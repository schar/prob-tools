{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lexica.Neo
import Lexica.Base
import LUM
import Prob
import Model (saUniv, saDom, gqUniv, gqDom, World)
import Lexica
import Vocab
import Language.Haskell.TH


{--

-- stage the types and priors for LUM over SA alternatives
------------------------------------------------------------------------------

-- define the GQ lexica that compete with Base
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
-- worlds, messages, and SA lexica
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

-- literal listener with baseLex
-- print the distribution over worlds for each possible message
dispL0 :: Show m => Lexicon m -> Params BDDist m -> [m] -> IO ()
dispL0 lex ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m ++ ", " ++ show lex) (l0 m lex ps) | m <- ms]

-- literal speaker with baseLex and gqMessage alternatives
-- print the distribution over messages for each possible world
dispS0 :: Show b => Lexicon b -> Params BDDist b -> [World] -> IO ()
dispS0 lex ps ws = sequence_ (map putStrLn test)
  where test = [prettyDist (show w ++ ", " ++ show lex) (s0 w lex ps) | w <- ws]

-- pragmatic listener with gqLexes and gqMessages as alternatives
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
