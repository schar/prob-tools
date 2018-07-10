
module LUM where

import Control.Monad             (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Function             (on)
import Data.List                 (partition)
import Lexica
import Vocab
-- import Model
import Prob
import Utils


-- RSA model parameters
data Params d m w = PM
  { worldPrior   :: d w -- distribution over worlds
  , messagePrior :: d m -- distribution over messages
  , lexiconPrior :: d (Lexicon m w) -- distribution over [[ ]] functions
  , cost         :: m -> Double -- message costs
  , temp         :: Double -- don't really know what this does
  }


-- Helper functions for scaling probabilities
scale :: m -> Params d m w -> BDDist a -> BDDist a
scale m model = modify (fmap $ exp . (temp model *) . subtract (cost model m) . log)

modify :: (Prob -> Prob) -> BDDist a -> BDDist a
modify f mx = MaybeT (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runBDDist mx]

-- Sum weights of identical outcomes
weightedEq :: (Dist m, Eq a) => [Mass Prob a] -> m a
weightedEq vs = weighted (concatMap col bins)
  where bins = groupEqBy ((==) `on` getSndMass) vs
        col []                = []
        col ms@((Mass _ x):_) = [Mass (sum (map getFstMass ms)) x]

groupEqBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupEqBy _ [] = []
groupEqBy f (a:rest) = (a:as) : groupEqBy f bs
  where (as,bs) = partition (f a) rest


listener :: (Eq w, Eq m) => Int -> m -> Lexicon m w -> Params BDDist m w -> BDDist w
listener 0 m lex model = bayes $
  do w <- worldPrior model
     guard (interpret lex m w)
     return w
listener 1 m _ model = bayes $
  do w <- worldPrior model
     lex' <- (lexiconPrior model)
     m' <- speaker 1 w lex' model
     guard (m' == m)
     return w
listener n m lex model = bayes $
  do w <- worldPrior model
     m' <- speaker n w lex model
     guard (m' == m)
     return w

speaker :: (Eq w, Eq m) => Int -> w -> Lexicon m w -> Params BDDist m w -> BDDist m
speaker n w lex model = bayes $
  do m <- messagePrior model
     w' <- scale m model (listener (n-1) m lex model)
     guard (w' == w)
     return m

{--

-- literal listener
------------------------------------------------------------------------------
-- given a message `m` and lexicon `lex`, return a distribution over worlds `w`
-- that could be described by `m` when interpreted by `lex`, weighted by world
-- prior
l0 :: Eq m => m -> Lexicon m -> Params BDDist m -> BDDist World
l0 m lex model = bayes $
  do w <- worldPrior model
     guard (interpret lex m w)
     return w

-- literal speaker
------------------------------------------------------------------------------
-- given a world `w` and lexicon `lex`, return a distribution over messages `m`
-- that are true of `w` when interpreted by `lex`, weighted by message prior and
-- cost
s1 :: Eq m => World -> Lexicon m -> Params BDDist m -> BDDist m
s1 w lex model = bayes $
  do m <- messagePrior model
     w' <- scale m model (l0 m lex model)
     guard (w' == w)
     return m

-- pragmatic listener
------------------------------------------------------------------------------
-- given a message `m`, return a distribution over worlds `w` compatible with
-- `m` under /some/ lexicon, weighted by world prior, lexicon prior,
-- and likelihood of s0 to describe `w` with `m`
l1 :: (Eq m) => m -> Params BDDist m -> BDDist World
l1 m model = bayes $
  do w <- worldPrior model
     sem <- (lexiconPrior model)
     m' <- s1 w sem model
     guard (m' == m)
     return w

--}

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
