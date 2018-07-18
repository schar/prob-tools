{-# LANGUAGE FlexibleContexts #-}

module Experiments.Scalar.SimpleScalar.LUM where

import Control.Monad             (MonadPlus, guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Class (lift)
import Data.Function             (on)
import Data.List                 (partition)
import Vocab
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

scale' :: (Dist d, MonadPlus d) => m -> Params d m w -> d a -> d a
scale' m model d = d >>= \x -> coin fct True False >>= guard >> return x
  where fct = Sum . exp $ (negate $ temp model) * (cost model m)

-- Helper functions for scaling probabilities
scale :: m -> Params d m w -> DDist a -> DDist a
scale m model = modify (fmap $ exp . (temp model *) . subtract (cost model m) . log)

modify :: (Prob -> Prob) -> DDist a -> DDist a
modify f mx = (MassT f'd)
  where f'd = [Mass (f n) x | Mass n x <- runMassT mx]

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


listener :: (Eq w, Eq m, Bayes d, Memo m (Lexicon m w) w d) => Int -> Params d m w -> m -> Lexicon m w -> d (DDist w)
listener 0 model m lex = bayes $
                         -- fmap (uniform . return) $
  do w <- worldPrior model
     guard (interpret lex m w)
     return w
listener 1 model m _ = bayes $
  do w <- worldPrior model
     lex' <- (lexiconPrior model)
     mdist <- speaker 1 model w lex'
     m' <- weighted $ runMassT mdist
     guard (m' == m)
     return w
listener n model m lex = bayes $
  do w <- worldPrior model
     mdist <- speaker n model w lex
     m' <- weighted $ runMassT mdist
     guard (m' == m)
     return w

speaker :: (Eq w, Eq m, Bayes d, Memo m (Lexicon m w) w d) => Int -> Params d m w -> w -> Lexicon m w -> d (DDist m)
speaker n model w lex = bayes $
                        -- fmap (uniform . return) $
  do m <- messagePrior model
     wdist <- scale m model <$> memo (listener (n-1) model) m lex
     w' <- unweighted $ runMassT wdist
     guard (w' == w)
     return m

-- literal listener with lex
-- print the distribution over worlds for each possible message
dispL0 :: (Eq m, Show m, Eq w, Show w, Bayes d, P d, Memo m (Lexicon m w) w d) => Lexicon m w -> Params d m w -> [m] -> IO ()
dispL0 lex ps ms = sequence_ (map printDist test)
  where test = [prettyDist (show m ++ ", " ++ show lex) . lift <$> listener 0 ps m lex | m <- ms]

-- literal speaker with lex
-- print the distribution over messages for each possible world
dispS1 :: (Eq m, Show m, Eq w, Show w, Bayes d, P d, Memo m (Lexicon m w) w d) => Lexicon m w -> Params d m w -> [w] -> IO ()
dispS1 lex ps ws = sequence_ (map printDist test)
  where test = [prettyDist (show w ++ ", " ++ show lex) . lift <$> speaker 1 ps w lex | w <- ws]

-- pragmatic listener summing over lexes
-- print the distribution over worlds (summing over lexica) for each possible message
dispL1 :: (Eq m, Show m, Eq w, Show w, Bayes d, P d, Memo m (Lexicon m w) w d) => Params d m w -> [m] -> IO ()
dispL1 ps ms = sequence_ (map printDist test)
  where test = [prettyDist (show m) . lift <$> listener 1 ps m undefined | m <- ms]

-- higher-order pragmatic speaker summing over lexes
-- print the distribution over messages for each possible world
dispSN :: (Eq m, Show m, Eq w, Show w, Bayes d, P d, Memo m (Lexicon m w) w d) => Int -> Params d m w -> [w] -> IO ()
dispSN n ps ws = sequence_ (map printDist test)
  where test = [prettyDist (show w) . lift <$> speaker n ps w undefined | w <- ws]

-- higher order pragmatic listener
-- print the distribution over worlds (iterating and then summing over lexica) for each possible message
dispLN :: (Eq m, Show m, Eq w, Show w, Bayes d, P d, Memo m (Lexicon m w) w d) => Int -> Params d m w -> [m] -> IO ()
dispLN n ps ms = sequence_ (map printDist test)
  where test = [prettyDist (show m) . lift <$> listener n ps m undefined | m <- ms]
