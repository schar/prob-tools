{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Prob where

import           Control.Applicative
import           Control.Monad             (MonadPlus, join, guard, replicateM)
import           Control.Monad.Trans.Class (lift, MonadTrans)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.List                 (foldl', nub, intercalate, group, sort, (\\))
import           Data.Maybe                (catMaybes, fromJust)
import           System.Random
import           Numeric                   (showFFloat)
import           Utils
import qualified Data.Map as M
-- import           System.IO.Memoize

breadth = 5000

-- Dist monads can represent lists of weighted of values
------------------------------------------------------------------------------
class Monad m => Dist m where
  weighted :: Eq a => [Mass Prob a] -> m a
  unweighted :: Eq a => [Mass Prob a] -> m a

-- Hist monads can be represented as lists of weighted of values/failures
------------------------------------------------------------------------------
class Dist m => Hist m where
  hist :: Eq a => m a -> IO [Mass Prob (Maybe a)]

uniform :: (Eq a, Dist m) => [a] -> m a
uniform = weighted . map (Mass 1)

coin :: (Eq a, Dist m) => Prob -> a -> a -> m a
coin n x1 x2 = weighted [Mass n x1, Mass (1-n) x2]

-- DDist: Discrete distributions
------------------------------------------------------------------------------
type DDist = MassT Prob []

instance (Eq a) => Eq (DDist a) where
  (MassT vs) == (MassT vs') = vs == vs'

instance Dist DDist where
  weighted vs = MassT $ map (\y -> Mass (mass y / total) y) (nub domain)
    where (mass, domain, total) = foldl' go (const 0, [], 0) vs
          go (f, dom, tot) (Mass p x) = (shift f x p, x : dom, tot + p)
          shift f x p = \y -> if y == x then f x + p else f y

  unweighted vs = MassT $ map (\y -> Mass (mass y) y) (nub domain)
    where (mass, domain) = foldl' go (const 0, []) vs
          go (f, dom) (Mass p x) = (shift f x p, x : dom)
          shift f x p = \y -> if y == x then f x + p else f y

instance Hist DDist where
  hist = return . runMassT . fmap Just . weighted . runMassT

-- MC: Sampling functions
------------------------------------------------------------------------------
class Dist d => Sampler d where
  randomDouble :: d Double

newtype MC a = MC { runMC :: IO a }
  deriving (Functor, Applicative, Monad)

instance Sampler MC where
  randomDouble = MC (getStdRandom (random))

normalize :: (Eq a, Dist m) => Double -> Double -> [(a, Double)] -> m a
normalize mean sd vals = weighted $ map (\(w, x) -> Mass (Sum $ norm mean sd x) w) vals
  where norm m s x = exp (- ((x - m) / s) ** 2 / 2) / (s * sqrt (2 * pi))

mkSampler :: Sampler d => DDist a -> d a
mkSampler ddist = randomDouble >>= \n -> pick (Sum n) (runMassT ddist)
  where pick _ []                          = fail "No values to pick from"
        pick n ((Mass m x):vs) | n <= m    = return x
                               | otherwise = pick (n-m) vs

instance Dist MC where
  weighted = mkSampler . weighted
  unweighted = mkSampler . unweighted

sample :: Dist m => Int -> m a -> m [a]
sample = replicateM

instance Hist MC where
  hist d = runMC $ runMassT . fmap Just . uniform <$> sample breadth d


-- `MaybeT m` distributions leave mass on states where conditions have failed
------------------------------------------------------------------------------
type BDDist = MaybeT DDist

runBDDist :: BDDist a -> [Mass Prob (Maybe a)]
runBDDist = runMassT . runMaybeT

instance Hist BDDist where
  hist = return . runMassT . weighted . runBDDist

instance {-# OVERLAPPING #-} Dist (MaybeT DDist) where
  weighted vs = lift $ weighted vs
  unweighted vs = lift $ unweighted vs
  -- weighted vs = MassT [Mass (n / total) x | Mass n x <- vs, total /= 0]
  --   where total = foldMap getFstMass vs

type BMC = MaybeT MC

instance Hist BMC where
  hist d = runMC $ runMassT . uniform <$> sample breadth (runMaybeT d)

instance Sampler m => Sampler (MaybeT m) where
  randomDouble = lift randomDouble

mkMaybeSampler :: (Sampler d, MonadPlus d) => DDist a -> d a
mkMaybeSampler ddist = randomDouble >>= \n -> pickMay (Sum n) (runMassT ddist)
  where pickMay _ []                          = empty
        pickMay n ((Mass m x):vs) | n <= m    = return x
                                  | otherwise = pickMay (n-m) vs

instance (Dist m, Sampler m) => Dist (MaybeT m) where
  weighted = mkMaybeSampler . weighted
  unweighted = mkMaybeSampler . unweighted

-- Bayesian filtering on DDists
------------------------------------------------------------------------------
-- remove states where conditions have failed and renormalize
bayesDD :: (Eq a, Dist m) => BDDist a -> m a
bayesDD = weighted . catMaybes . fmap pull . runBDDist
  where pull (Mass y mx) = do {x <- mx; return (Mass y x)}

-- Bayesian filtering on MC samplers
------------------------------------------------------------------------------
sampleWithRejections :: Dist m => Int -> MaybeT m a -> m [a]
sampleWithRejections n d = fmap catMaybes (sample n (runMaybeT d))

bayesMC :: (Dist m, Eq a) => Int -> MaybeT m a -> m (DDist a)
bayesMC n v = uniform <$> sampleWithRejections n v

class (Dist m, Alternative m) => Bayes m where
  bayes :: Eq a => m a -> m (DDist a)

instance Bayes BDDist where
  bayes = return . bayesDD

instance Bayes BMC where
  bayes = lift . bayesMC breadth

-- tools for memoization
------------------------------------------------------------------------------
instance Sampler m => Sampler (StateT s m) where
  randomDouble = lift randomDouble

instance Dist m => Dist (StateT s m) where
  weighted = lift . weighted
  unweighted = lift . unweighted

type Cache k l v = M.Map (k, l) (DDist v)
type MemoBMC k l v = MaybeT (StateT (Cache k l v) MC)

instance Bayes (MemoBMC k l v) where
  bayes = lift . bayesMC breadth

instance Hist (MemoBMC k l v) where
  hist d = runMC . flip evalStateT M.empty $ runMassT . uniform <$> sample breadth (runMaybeT d)

class Memo k l v d where
  memo :: (k -> l -> d (DDist v)) -> k -> l -> d (DDist v)

instance (Ord k, Ord l) => Memo k l v (MemoBMC k l v) where
  memo f k l = do
    c <- lift get
    mr <- return (M.lookup (k, l) c)
    case mr of
      Just r -> return r
      Nothing -> do r <- f k l
                    lift $ put (M.insert (k, l) r c)
                    return r

instance Memo k l v BDDist where
  memo = id

-- Flu test
------------------------------------------------------------------------------
data Status = Flu | Healthy
              deriving (Show, Eq, Ord)
data Test   = Pos | Neg
              deriving (Show, Eq, Ord)

statusCondPos :: (Dist m, Alternative m) => m (Status, Test)
statusCondPos = do
  status <- coin 0.10 Flu Healthy
  test   <- coin (case status of {Flu -> 0.7; Healthy -> 0.1}) Pos Neg
  guard (test == Pos)
  return (status, test)

test1 :: IO [Mass Prob (Maybe (Status, Test))]
test1 = hist (statusCondPos :: BDDist (Status,Test))

test2 :: IO [Mass Prob (Maybe (Status, Test))]
test2 = hist (statusCondPos :: BMC (Status,Test))
  -- do vs <- hist statusCondPos
  --    return $ runMassT (uniform vs)

test3 :: IO [Mass Prob (Status, Test)]
test3 = return . runMassT $ bayesDD (statusCondPos :: BDDist (Status,Test))

test4 :: IO [Mass Prob (Status, Test)]
test4 = runMC $ do
  ddist <- bayesMC breadth (statusCondPos :: BMC (Status,Test))
  return $ runMassT ddist

test5 :: IO [(Status, Test)]
test5 = runMC $ do
  ddist <- bayesMC breadth (statusCondPos :: BMC (Status,Test))
  ws <- return $ runMassT ddist
  sample 10 (weighted ws)


-- helper functions for displaying distributions
------------------------------------------------------------------------------
prettyDist :: Show b => String -> BDDist b -> String
prettyDist o mx = "P(.|" ++ o ++ "): " ++ intercalate ", " (go <$> runBDDist mx)
  where go (Mass (Sum n) v) = maybe "Nothing" show v ++ " = " ++ showFFloat (Just 2) n ""

prettyDistN :: Show b => Int -> String -> BDDist b -> String
prettyDistN r o mx = "P(.|" ++ o ++ "): " ++ intercalate ", " (go <$> runBDDist mx)
  where go (Mass (Sum n) v) = maybe "Nothing" show v ++ " = " ++ showFFloat (Just r) n ""

instance Show a => Show (DDist a) where
  show = prettyDist "" . lift

class Dist d => P d where
  printDist :: Show a => d a -> IO ()

instance P DDist where
  printDist = print . runMassT

instance P BDDist where
  printDist = print . (\case {(Mass p (Just v)) -> v}) . head . runBDDist

instance P MC where
  printDist = join . fmap print . runMC

instance P BMC where
  printDist = join . fmap (print . fromJust) . runMC . runMaybeT

instance (Show k, Show l, Show v) => P (MemoBMC k l v) where
  printDist = join . fmap (print . fromJust) . runMC . flip evalStateT M.empty . runMaybeT
  -- if you want to see the memoized maps from lower-level agents, then:
  -- > printDist = join . fmap (print . (\(x,c) -> (fromJust x, c))) . runMC . flip runStateT M.empty . runMaybeT
