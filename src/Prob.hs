{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prob where

import           Control.Applicative
import           Control.Monad             (join, guard, replicateM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
-- import qualified Data.HashMap.Lazy as M
import           Data.List                 (foldl', nub, intercalate, group, sort, (\\))
import           Data.Maybe                (catMaybes)
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Dist
import           Numeric                   (showFFloat)
import           Utils

--
-- Dist monads support normalization
--

class Monad m => Dist m where
  weighted :: Eq a => [Mass Prob a] -> m a

uniform :: (Eq a, Dist m) => [a] -> m a
uniform = weighted . map (Mass 1)

coin :: (Eq a, Dist m) => Prob -> a -> a -> m a
coin n x1 x2 = weighted [Mass n x1, Mass (1-n) x2]

--
-- DDist
--

type DDist = MassT Prob []

instance Dist DDist where
  weighted vs = MassT $ map (\y -> Mass (mass y / total) y) (nub domain)
    where (mass, domain, total) = foldl' go (const 0, [], 0) vs
          go (f, dom, tot) (Mass p x) = (shift f x p, x : dom, tot + p)
          shift f x p = \y -> if y == x then f x + p else f y
  -- weighted vs = MassT [Mass (n / total) x | Mass n x <- vs, total /= 0]
  --   where total = foldMap getFstMass vs

--
-- MC sampling
--

newtype MC a = MC { runMC :: IO a }
  deriving (Functor, Applicative, Monad)

uniformDouble :: MC Double
uniformDouble = MC $ Rand.create >>= Rand.uniform

normalize :: (Eq a, Dist m) => Double -> Double -> [(a, Double)] -> m a
normalize mean sd vals = weighted $ map (\(w, x) -> Mass (Sum $ norm mean sd x) w) vals
  where norm m s x = exp (- ((x - m) / s) ** 2 / 2) / (s * sqrt (2 * pi))

instance Dist MC where
  weighted = liftF . weighted

liftF :: DDist a -> MC a
liftF ddist = do
  n <- uniformDouble
  pick (Sum n) (runMassT ddist)

pick :: Dist m => Prob -> [Mass Prob a] -> m a
pick _ [] = fail "No values to pick from"
pick n ((Mass m x):vs)
  | n <= m    = return x
  | otherwise = pick (n-m) vs

sample :: Int -> MC a -> MC [a]
sample = replicateM

--
-- Bayesian filtering
--

instance Dist m => Dist (MaybeT m) where
  -- weighted = MaybeT . fmap Just . weighted
  weighted = lift . weighted

--
-- Bayesian DDist
--

type BDDist = MaybeT DDist

runBDDist :: BDDist a -> [Mass Prob (Maybe a)]
runBDDist = runMassT . runMaybeT

bayes :: (Eq a, Dist m) => BDDist a -> m a
bayes = weighted . catMaybes . fmap pull . runBDDist
  where -- catMaybes' = catMaybes . fmap pull
        pull (Mass y mx) = do {x <- mx; return (Mass y x)}

--
-- Bayesian MC
--

type BMC = MaybeT MC

sampleWithRejections :: Int -> BMC a -> IO [a]
sampleWithRejections n d = fmap catMaybes (runMC $ sample n (runMaybeT d))

bayesMC :: (Eq a, Dist m) => Int -> BMC a -> IO (m a)
bayesMC n v = uniform <$> sampleWithRejections n v

-- bayesMC :: Ord a => Int -> BMC a -> MC a
-- bayesMC n v = fmap hist (sampleWithRejections n v)
--   where hist = map (\xs -> Mass (Sum $ length xs) (head xs)) . group . sort

--
-- Flu test
--

data Status = Flu | Healthy
  deriving (Show, Eq, Ord)
data Test   = Pos | Neg
  deriving (Show, Eq, Ord)

statusCondPos :: (Dist m, Alternative m) => m (Status, Test)
statusCondPos = do
  status <- coin 0.10 Flu Healthy
  test   <-
    if (status == Flu)
      then coin 0.70 Pos Neg
      else coin 0.10 Pos Neg
  guard (test == Pos)
  return (status, test)

test1 :: [Mass Prob (Status, Test)]
test1 = runMassT (bayes statusCondPos)

test2 :: IO [Mass Prob (Status, Test)]
test2 = do ddist <- bayesMC 10000 statusCondPos
           return $ runMassT (ddist :: DDist (Status, Test))

test3 :: IO [(Status, Test)]
test3 = runMC . sample 10 . join . MC $ bayesMC 10000 statusCondPos
           -- print $ runMassT (ddist :: DDist (Status, Test))



-- helper function for displaying distributions
prettyDist :: Show b => String -> BDDist b -> String
prettyDist o mx = "P(.|" ++ o ++ "): " ++ intercalate ", " (go <$> runBDDist mx)
  where go = \(Mass (Sum n) (Just x)) -> show x ++ " = " ++ showFFloat (Just 2) n ""
        -- prettyN (Sum n) = showFFloat (Just 2) n ""
