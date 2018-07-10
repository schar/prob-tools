{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Experiments.Scalar.SimpleScalar.Lexica where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import LUM
import Lexica
import Lexica.ParseTree
import Vocab
import Experiments.Scalar.SimpleScalar.Domain
import Control.Monad (MonadPlus, forM, filterM)
import Data.List (intersect, nub, subsequences, (\\))


type Prop = World -> Bool
type family TypeOf a where
  TypeOf S  = Prop
  TypeOf NP = Entity
  TypeOf VP = World -> [Entity]
  TypeOf TV = World -> [(Entity,Entity)]

class Eval f where
  eval   :: f S -> Prop

newtype SAMessage = SAMessage (forall f. (Grammar f, NameLex f, SALex f) => f S)
instance Eq SAMessage where
  (SAMessage m) == (SAMessage m') = (m :: ParseTree S) == m'
instance Ord SAMessage where
  compare (SAMessage m) (SAMessage m') = compare (m :: ParseTree S) m'
instance Show SAMessage where
  show (SAMessage m) = show (m :: ParseTree S)

-- base lexicon

data Base a = B {runBase :: (TypeOf a)}

instance Grammar Base where
  s (B x) (B f)     = B $ \w -> x `elem` f w
  tvp (B f) (B x)   = B $ \w -> [y | (x,y) <- f w]
  nil               = B $ const True

instance Eval Base where
  eval              = runBase

instance NameLex Base where
  john              = B $ John
  mary              = B $ Mary

instance SALex Base where
  scored            = B $ \w -> nub [x | y <- shot' w, (x,y) <- hit' w]
  aced              = B $ \w -> nub [x | (x,_) <- hit' w, shot' w == shot' w `intersect` [y | (z,y) <- hit' w, z==x]]

instance GQLex Base where
  johnQ q           = q john
  maryQ q           = q mary
  noPlayer q        = B $ \w -> not (any (\y -> eval (q (B y)) w) (player' w))
  somePlayer q      = B $ \w -> any (\y -> eval (q (B y)) w) (player' w)
  everyPlayer q     = B $ \w -> all (\y -> eval (q (B y)) w) (player' w)

instance MannerLex Base where
  started           = B $ const True
  gotStarted        = B $ const True

baseSALex :: Lexicon SAMessage World
baseSALex = Lexicon "Base" (\(SAMessage m) -> runBase m)

-- macros to generate lexicon instances for refinements of Base
------------------------------------------------------------------------------

-- characteristic sets representing an SA refinement, given a concrete model
data SADict = SADict
  { scored' :: [(Entity,World)]
  , aced'   :: [(Entity,World)]
  }
  deriving (Eq, Show, Lift)

-- at a concrete model with domain of entities `dom` and domain of worlds `univ`,
-- generate all possible refinements of the Base lexicon
refineSABase :: [Entity] -> [World] -> [SADict]
refineSABase dom univ =
  let pset    = tail . subsequences
                -- powerset-plus function
      pScored = pset [(x,w) | w <- univ, x <- runBase scored w]
                -- powerset of Base's interpretation of "scored"
      pAced   = pset [(x,w) | w <- univ, x <- runBase aced   w]
                -- powerset of Base's interpretation of "aced"
   in [ SADict {scored' = sc, aced' = ac} | sc <- pScored, ac <- pAced ]

-- declare a lexicon type called `name`
genData :: Name -> Q Dec
genData name = dataD (cxt []) name    vars Nothing   fields             derives
            -- data           LexName a            = LexName (TypeOf a)
  where a       = mkName "a"
        vars    = [PlainTV a]
        b       = bang noSourceUnpackedness noSourceStrictness
        fields  = [normalC name [bangType b [t| TypeOf $(varT a) |]]]
        derives = []

-- given a refinement dictionary, declare the evaluation algebras that define
-- the `name` lexicon
deriveSALex :: Name -> SADict -> Q [Dec]
deriveSALex name sadict = do
  ddec <- genData name
  idec <- [d| instance Grammar $t where
                s $px $pf   = $d $ \w -> x `elem` f w
                tvp $pf $px = $d $ \w -> [y | (z,y) <- f w, z == x]
                nil         = $d $ const True

              instance Eval $t where
                eval ($px)  = x

              instance NameLex $t where
                john        = $d John
                mary        = $d Mary

              instance SALex $t where
                scored      = $d $ \w -> [x | (x,v) <- scored' sadict, v == w]
                aced        = $d $ \w -> [x | (x,v) <- aced'   sadict, v == w]

              instance GQLex $t where
                johnQ       q   = q john
                maryQ       q   = q mary
                noPlayer    q   = $d $ \w -> not (any (\y -> eval (q ($d y)) w) (player' w))
                somePlayer  q   = $d $ \w -> any (\y -> eval (q ($d y)) w) (player' w)
                everyPlayer q   = $d $ \w -> all (\y -> eval (q ($d y)) w) (player' w)
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]

-- declare lexica for all possible refinements of Base
mkSALexes :: Q [Dec]
mkSALexes = do (ds, es) <- ldecs
               ls       <- valD (varP (mkName "saLexes")) (normalB $ listE es) []
                        -- effectively:
                        --   saLexes = [(\m -> eval (open m :: l S)) | l <- refineBase ...]
               return $ ds ++ [ls]

  where lexes  = refineSABase saDom saUniv
                 -- all refinements of Base under the saDom, saUniv model
        ldecs  = foldr genLex (return ([],[])) lexes
                 -- for each refinement in lexes, declare evaluation behavior,
                 -- and return a Lexicon object reifying the evaluation function
        genLex = \sadict accum ->
          do (decs, evs) <- accum
             sn          <- show <$> newName "SA"
             n           <- return (mkName sn)
             newdecs     <- deriveSALex n sadict
             let ev      = [| Lexicon sn       (\(SAMessage m) -> eval (m :: $(conT n) S)) |]
                           -- Lexicon "SA_123" (\m -> eval (open m :: SA_123    S))
             return (decs ++ newdecs, evs ++ [ev])
