{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Utils (
  Mass(Mass, getFstMass, getSndMass),
  MassT(MassT, runMassT),
  Sum(Sum, getSum),
  Prob
) where

import           Control.Monad             (ap, liftM2)
import           Control.Applicative
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Monoid
import           Data.Semiring
import           Numeric                   (showFFloat)

type Prob = Sum Double

data Mass w a = Mass { getFstMass :: w, getSndMass :: a }
  deriving (Functor)

instance (Show a) => Show (Mass Prob a) where
  show (Mass (Sum p) a) = show a ++ ": " ++ showFFloat (Just 2) p ""

instance Semiring w => Monad (Mass w) where
  return x = Mass one x
  m >>= f  = Mass (w1 <.> w2) x
    where (Mass w1 (Mass w2 x)) = fmap f m

instance Semiring w => Applicative (Mass w) where
  pure  = return
  (<*>) = ap

newtype MassT w m a = MassT { runMassT ::  m (Mass w a) }
  deriving Functor

instance (Semiring w, Monad m) => Monad (MassT w m) where
  return   = MassT . return . return
  mx >>= f = MassT $ do
    Mass w1 x1 <- runMassT mx
    Mass w2 x2 <- runMassT (f x1)
    return (Mass (w1 <.> w2) x2)

instance Semiring w => MonadTrans (MassT w) where
  lift = MassT . fmap return

instance (Semiring w, Monad m) => Applicative (MassT w m) where
  pure  = return
  (<*>) = ap

instance (Semiring w, Monad m, Alternative m) => Alternative (MassT w m) where
  empty = MassT empty
  m <|> n = MassT $ runMassT m <|> runMassT n

instance Num a => Semiring (Sum a) where
  one   = Sum 1
  (<.>) = (*)

instance Fractional a => Fractional (Sum a)
  where fromRational  = Sum . fromRational
        (/) = liftM2 (/) -- (<*>) . ((/) <$>)

-- instance Floating a => Floating (Sum a)
--   where pi    = Sum pi
--         exp   = fmap exp
--         log   = fmap log
--         sin   = fmap sin
--         cos   = fmap cos
--         asin  = fmap asin
--         acos  = fmap acos
--         atan  = fmap atan
--         sinh  = fmap sinh
--         cosh  = fmap cosh
--         asinh = fmap asinh
--         acosh = fmap acosh
--         atanh = fmap atanh -- ugh
