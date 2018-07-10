
module Experiments where

import LUM
import Prob
import Model
import Lexica

-- literal listener with lex
-- print the distribution over worlds for each possible message
dispL0 :: (Eq m, Show m, Eq w, Show w) => Lexicon m w -> Params BDDist m w -> [m] -> IO ()
dispL0 lex ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m ++ ", " ++ show lex) (listener 0 m lex ps) | m <- ms]

-- literal speaker with lex
-- print the distribution over messages for each possible world
dispS1 :: (Eq b, Show b, Eq w, Show w) => Lexicon b w -> Params BDDist b w -> [w] -> IO ()
dispS1 lex ps ws = sequence_ (map putStrLn test)
  where test = [prettyDist (show w ++ ", " ++ show lex) (speaker 1 w lex ps) | w <- ws]

-- pragmatic listener summing over lexes
-- print the distribution over worlds (summing over lexica) for each possible message
dispL1 :: (Eq m, Show m, Eq w, Show w) => Params BDDist m w -> [m] -> IO ()
dispL1 ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m) (listener 1 m undefined ps) | m <- ms]

-- higher-order pragmatic speaker summing over lexes
-- print the distribution over messages for each possible world
dispSN :: (Eq b, Show b, Eq w, Show w) => Int -> Params BDDist b w -> [w] -> IO ()
dispSN n ps ws = sequence_ (map putStrLn test)
  where test = [prettyDist (show w) (speaker n w undefined ps) | w <- ws]

-- higher order pragmatic listener
-- print the distribution over worlds (iterating and then summing over lexica) for each possible message
dispLN :: (Eq m, Show m, Eq w, Show w) => Int -> Params BDDist m w -> [m] -> IO ()
dispLN n ps ms = sequence_ (map putStrLn test)
  where test = [prettyDist (show m) (listener n m undefined ps) | m <- ms]
