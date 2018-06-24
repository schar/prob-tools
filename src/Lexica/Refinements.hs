{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica.Refinements where

import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate, nub, intersect)


{--

data Refined m a = Ref {runRefined :: (m (Base a))}

instance (Applicative m) => NameLex (Refined m) where
  john                = Ref $ pure john
  mary                = Ref $ pure mary

instance (Applicative m) => Grammar (Refined m) where
  s (Ref x) (Ref f)   = Ref (s <$> x <*> f)
  tvp (Ref f) (Ref x) = Ref (tvp <$> f <*> x)

refineET :: (Foldable m, MonadPlus m) =>
            (Entity -> Prop) -> m Entity -> m World -> m (Entity -> Prop)
refineET q dom univ =
  let m = do x <- dom
             w <- univ
             guard $ q x w
             return (x,w)
      -- ms = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) [True,False]) (pure []) m
      ms = (\\ [[]]) $ filterM (const [True,False]) (toList m)
  in  msum $ fmap (\m' -> pure $ \x w -> (x,w) `elem` m') ms

instance (Applicative m, Foldable m, MonadPlus m) => SALex (Refined m) where
  scored = Ref $ fmap B $ refineET (runBase scored) empty empty
  aced = Ref $ pure aced

--}
