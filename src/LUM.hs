
module LUM where

import Control.Monad             (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Function             (on)
import Data.List                 (partition)
import Lexica
import Vocab
import Model
import Prob
import Utils


-- RSA model parameters
data Params d m = PM
  { worldPrior   :: d World -- distribution over worlds
  , messagePrior :: d m -- distribution over messages
  , lexiconPrior :: d (Lexicon m) -- distribution over [[ ]] functions
  , cost         :: m -> Double -- message costs
  , temp         :: Double -- don't really know what this does
  }


-- Helper functions for scaling probabilities
scale :: m -> Params d m -> BDDist a -> BDDist a
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


listener :: Eq m => Int -> m -> Lexicon m -> Params BDDist m -> BDDist World
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

speaker :: Eq m => Int -> World -> Lexicon m -> Params BDDist m -> BDDist m
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
