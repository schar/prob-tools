{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lexica
import LUM
import Prob
import Model (saUniv, saDom, gqUniv, gqDom)
import TH    (mkLexes, mkSALexes, mkGQLexes)
import Language.Haskell.TH

-- $(fmap (:[]) $ valD (varP (mkName "gqInventory")) (mkLexes "GQ" >>= \ds -> normalB $ litE $ IntegerL $ toInteger $ length ds) [])
-- valD (varP (mkName "gqLexes")) (normalB $ listE $ fmap snd ldecs) []

main :: IO ()
main = return ()

{--

-- stage the types and priors for LUM over SA alternatives
------------------------------------------------------------------------------

-- define the GQ lexica that compete with Base
$(mkGQLexes)
-- from TH: gqLexes = [\m -> eval (open m :: l S) | l <- refineBase ...]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and SA lexica
saParams :: Dist m => Params m
saParams = PM
  { worldPrior   = uniform saUniv
  , messagePrior = uniform saMessages
  , lexiconPrior = uniform saLexes
  , cost         = \x -> if x == Message nil then 5 else 0
  , temp         = 1
  }

-- specify the alternative utterances
saMessages :: [Message]
saMessages = [Message (s john scored), Message (s john aced), Message nil]


-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

-- literal listener with baseLex
-- print the distribution over worlds for each possible message
dispL0 = sequence_ (map putStrLn test)
  where test = [prettyDist (show m ++ ", baseLex") (l0 m baseLex saParams) | m <- saMessages]

-- literal speaker with baseLex and saMessage alternatives
-- print the distribution over messages for each possible world
dispS0 = sequence_ (map putStrLn test)
  where test = [prettyDist (show w ++ ", baseLex") (s0 w baseLex saParams) | w <- saUniv]

-- pragmatic listener with saLexes and saMessages as alternatives
-- print the distribution over worlds (summing over lexica) for each possible message
dispL1 = sequence_ (map putStrLn test)
  where test = [prettyDist (show m) (l1 m saParams) | m <- saMessages]

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispL0

  putStrLn ""
  putStrLn "S0"
  putStrLn "----------"
  dispS0

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispL1

  {--
  L0
  ----------
  P(.|John scored, baseLex): wA = 0.5, wS = 0.5
  P(.|John aced, baseLex): wA = 1.0
  P(.|Silence, baseLex): wA = 0.33, wS = 0.33, wN = 0.33

  S0
  ----------
  P(.|wA, baseLex): John scored = 0.33, John aced = 0.67, Silence = 0.0
  P(.|wS, baseLex): John scored = 1.0, Silence = 0.0
  P(.|wN, baseLex): Silence = 1.0

  L1
  ----------
  P(.|John scored): wA = 0.29, wS = 0.71
  P(.|John aced): wA = 1.0
  P(.|Silence): wA = 0.0, wS = 0.25, wN = 0.75
  --}

--}

{--

-- stage the types and priors for LUM over GQ alternatives
------------------------------------------------------------------------------

-- define the GQ lexica that compete with Base
$(mkGQLexes)
-- from TH: gqLexes = [\m -> eval (open m :: l S) | l <- refineBase ...]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and SA lexica
gqParams :: Dist m => Params m
gqParams = PM
  { worldPrior   = uniform gqUniv
  , messagePrior = uniform gqMessages
  , lexiconPrior = uniform gqLexes
  , cost         = \x -> if x == Message nil then 5 else 0
  , temp         = 1
  }

-- specify the alternative utterances
gqMessages :: [Message]
gqMessages = [Message (some player (\x -> s x scored)), Message (every player (\x -> s x scored)), Message nil]

-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

-- literal listener with baseLex
-- print the distribution over worlds for each possible message
dispL0 = sequence_ (map putStrLn test)
  where test = [prettyDist (show m ++ ", baseLex") (l0 m baseLex gqParams) | m <- gqMessages]

-- literal speaker with baseLex and gqMessage alternatives
-- print the distribution over messages for each possible world
dispS0 = sequence_ (map putStrLn test)
  where test = [prettyDist (show w ++ ", baseLex") (s0 w baseLex gqParams) | w <- gqUniv]

-- pragmatic listener with gqLexes and gqMessages as alternatives
-- print the distribution over worlds (summing over lexica) for each possible message
dispL1 = sequence_ (map putStrLn test)
  where test = [prettyDist (show m) (l1 m gqParams) | m <- gqMessages]

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispL0

  putStrLn ""
  putStrLn "S0"
  putStrLn "----------"
  dispS0

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispL1

--}


{--
testLex :: Message -> [Lexicon] -> [World] -> IO ()
testLex message lexes univ =
  forM_ lexes (\(Lexicon n lex) -> do
                 putStrLn ""
                 putStr n
                 putStr "::: "
                 forM_ univ (\w -> do
                                putStr (show w)
                                putStr ": "
                                putStr (show $ lex message w)
                                putStr ", ")
                 putStrLn "")
--}
