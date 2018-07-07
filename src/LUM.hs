
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

-- instance Message m => Eq m where
--   m == m' = (open m :: ParseTree S) == (m' :: ParseTree S)
-- instance Message m => Ord m where
--   compare m m' = compare (open m :: ParseTree S) (open m' :: ParseTree S)
-- instance Message m => Show m where
--   show m = show (open m :: ParseTree S)


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


-- literal listener
------------------------------------------------------------------------------
-- given a message `m` and lexicon `lex`, return a distribution over worlds `w`
-- that could be described by `m` when interpreted by `lex`, weighted by world
-- prior
l0 :: Eq m => m -> Lexicon m -> Params BDDist m -> BDDist World
l0 m lex model = bayes $ do
  w <- worldPrior model
  guard (interpret lex m w)
  return w

-- literal speaker
------------------------------------------------------------------------------
-- given a world `w` and lexicon `lex`, return a distribution over messages `m`
-- that are true of `w` when interpreted by `lex`, weighted by message prior and
-- cost
s0 :: Eq m => World -> Lexicon m -> Params BDDist m -> BDDist m
s0 w lex model = bayes $ do
  m <- messagePrior model
  w' <- scale m model (l0 m lex model)
  guard (w' == w)
  return m

-- pragmatic listener
------------------------------------------------------------------------------
-- given a message `m`, return a distribution over worlds `w` compatible with
-- `m` under /some/ lexicon, weighted by world prior, lexicon prior,
-- and likelihood of s0 to describe `w` with `m`
l1 :: (Eq m) => m -> Params BDDist m -> BDDist World
l1 m model = weightedEq . runMassT . bayes $ do
  w <- worldPrior model
  sem <- (lexiconPrior model)
  m' <- s0 w sem model
  guard (m' == m)
  return w
