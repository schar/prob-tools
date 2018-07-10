{-# LANGUAGE TemplateHaskell #-}

module Experiments.Scalar.RefinementsForDays.Trials where

import Experiments.Scalar.RefinementsForDays.Domain
import Experiments.Scalar.RefinementsForDays.Lexica
import LUM
import Prob
import Vocab


-- TOO MANY LEXICA TO COMPILE! :(

{--

-- stage the types and priors for LUM over Neo alternatives
------------------------------------------------------------------------------

-- define the Neo lexica that compete with Base
$(mkGQLexes)
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
  , lexiconPrior = uniform gqLexes
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

--}
