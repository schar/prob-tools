{-# LANGUAGE TemplateHaskell #-}

module Experiments.Manner where

import Lexica.Manner
import Lexica.Base
import LUM
import Prob
import Utils
import Model
import Vocab
import Experiments

{--}

-- stage the types and priors for LUM over SA alternatives
------------------------------------------------------------------------------

-- define the Manner lexica that compete with Base
$(mkMannerLexes)
-- from Lexica.Manner: mannerLexes = [\m -> eval (m :: l S) | l <- refineBase ...]

baselex = baseMannerLex
universe = mannerUniv

-- specify the alternative utterances
messages :: [MannerMessage]
messages =
  [ MannerMessage (started)
  , MannerMessage (gotStarted)
  , MannerMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and SA lexica
params :: Dist d => Params d MannerMessage
params = PM
  { worldPrior   = weighted $ zipWith Mass [2, 1] universe
  , messagePrior = uniform messages
  , lexiconPrior = uniform mannerLexes
  , cost         = \x -> case (lookup x (zip messages [1,2,5])) of {Just c -> c}
  , temp         = 4
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

  putStrLn ""
  putStrLn "S2"
  putStrLn "----------"
  dispSN 2 params universe

  putStrLn ""
  putStrLn "L2"
  putStrLn "----------"
  dispLN 2 params messages

  putStrLn ""
  putStrLn "S3"
  putStrLn "----------"
  dispSN 3 params universe

  putStrLn ""
  putStrLn "L3"
  putStrLn "----------"
  dispLN 3 params messages
