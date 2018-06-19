{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Lexica where

import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (intercalate)

-- categories for the object language algebra
data NP
data VP
data S
data TV

type Prop = World -> Bool


-- Lex classes define terms for different kinds of obj language expressions
------------------------------------------------------------------------------
class Grammar f where
  s      :: f NP -> f VP -> f S
  tvp    :: f TV -> f NP -> f VP
  nil    :: f S

class Eval f where
  eval   :: f S -> Prop

class NameLex f where
  john   :: f NP
  mary   :: f NP

class (NameLex f) => SALex f where
  aced   :: f VP
  scored :: f VP

-- a message is an unevaluated obj language term of category S
------------------------------------------------------------------------------
newtype Message = Message
  { open :: forall f. (Grammar f, Eval f, NameLex f, SALex f) => f S }
instance Eq Message where
  (Message m) == (Message m') = (m :: ParseTree S) == (m' :: ParseTree S)
instance Ord Message where
  compare (Message m) (Message m') = compare (m :: ParseTree S) (m' :: ParseTree S)
instance Show Message where
  show m = show (open m :: ParseTree S)


-- a lexicon is an algebra that evaluates object language terms
------------------------------------------------------------------------------

-- first lexicon: ParseTree carries terms to trees of strings
------------------------------------------------------------------------------
instance (Ord a) => Ord (Tree a) where
  compare = compare1
data ParseTree a = Nil | PT (Tree String) deriving (Eq, Ord)
instance Show (ParseTree a) where
  show (PT tree) = foldTree (\x cs -> if null cs then x else intercalate " " cs) tree
  show Nil       = "Silence"

instance Grammar ParseTree where
  s (PT x) (PT f)   = PT $ Node "" [x,f]
  tvp (PT f) (PT x) = PT $ Node "" [f,x]
  nil               = Nil

instance Eval ParseTree where
  eval _ _          = True

instance NameLex ParseTree where
  john              = PT $ pure "John"
  mary              = PT $ pure "Mary"

instance SALex ParseTree where
  aced              = PT $ pure "aced"
  scored            = PT $ pure "scored"



-- second lexicon: Base carries terms to familiar e/s/t denotations
------------------------------------------------------------------------------
type family TypeOf a where
  TypeOf S  = Prop
  TypeOf NP = Entity
  TypeOf VP = Entity -> Prop
  TypeOf TV = Entity -> Entity -> Prop

data Base a = B {runBase :: (TypeOf a)}

instance Grammar Base where
  s (B x) (B f)     = B $ f x
  tvp (B f) (B x)   = B $ f x
  nil               = B $ const True

instance Eval Base where
  eval              = runBase

instance NameLex Base where
  john              = B $ John
  mary              = B $ Mary

instance SALex Base where
  scored            = B $ \x w -> any (\y -> (x,y) `elem` hit' w) (shot' w)
  aced              = B $ \x w -> all (\y -> (x,y) `elem` hit' w) (shot' w)

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
