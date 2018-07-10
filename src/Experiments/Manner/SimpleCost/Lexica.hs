{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Experiments.Manner.SimpleCost.Lexica where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lexica
import Lexica.ParseTree
import Vocab
import Experiments.Manner.SimpleCost.Domain
-- import Data.Tree
-- import Data.Functor.Classes (compare1)
import Data.List            (subsequences, intercalate, nub, intersect)

type Prop = World -> Bool
type family TypeOf a where
  TypeOf S  = Prop
  TypeOf NP = Entity
  TypeOf VP = World -> [Entity]
  TypeOf TV = World -> [(Entity,Entity)]

class Eval f where
  eval   :: f S -> Prop

-- a message is an unevaluated obj language term of category S
------------------------------------------------------------------------------
newtype MannerMessage = MannerMessage (forall f. (MannerLex f) => f S)
instance Eq MannerMessage where
  (MannerMessage m) == (MannerMessage m') = (m :: ParseTree S) == m'
instance Ord MannerMessage where
  compare (MannerMessage m) (MannerMessage m') = compare (m :: ParseTree S) m'
instance Show MannerMessage where
  show (MannerMessage m) = show (m :: ParseTree S)

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

baseMannerLex :: Lexicon MannerMessage World
baseMannerLex = Lexicon "Base" (\(MannerMessage m) -> runBase m)

-- The Manner lexicon interprets terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------

data MannerDict = MannerDict
  { started' :: [World]
  , gotStarted' :: [World]
  }
  deriving (Eq, Show, Lift)

-- at a concrete model with domain of entities `dom` and domain of worlds `univ`,
-- generate all possible refinements of the Base lexicon
refineMannerBase :: [World] -> [MannerDict]
refineMannerBase univ =
  let pset    = tail . subsequences
                -- powerset-plus function
      pStarted = pset [w | w <- univ, runBase started w]
                -- powerset of Base's interpretation of "scored"
      pGotStarted = pset [w | w <- univ, runBase gotStarted w]
                -- powerset of Base's interpretation of "aced"
   in [ MannerDict {started' = st, gotStarted' = gs} | st <- pStarted, gs <- pGotStarted ]


-- declare a lexicon type called `name`
genData :: Name -> Q Dec
genData name = dataD (cxt []) name    vars Nothing   fields             derives
            -- data           LexName a            = LexName (TypeOf a)
  where a       = mkName "a"
        vars    = [PlainTV a]
        b       = bang noSourceUnpackedness noSourceStrictness
        fields  = [normalC name [bangType b [t| TypeOf $(varT a) |]]]
        derives = []

deriveMannerLex :: Name -> MannerDict -> Q [Dec]
deriveMannerLex name mannerdict = do
  ddec <- genData name
  idec <- [d| instance Grammar $t where
                s $px $pf   = $d $ \w -> x `elem` f w
                tvp $pf $px = $d $ \w -> [y | (z,y) <- f w, z == x]
                nil         = $d $ const True

              instance Eval $t where
                eval ($px)  = x

              instance MannerLex $t where
                started     = $d $ \w -> w `elem` started' mannerdict
                gotStarted  = $d $ \w -> w `elem` gotStarted' mannerdict
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]

-- declare lexica for all possible refinements of Base
mkMannerLexes :: Q [Dec]
mkMannerLexes = do (ds, es) <- ldecs
                   ls       <- valD (varP (mkName "mannerLexes")) (normalB $ listE es) []
                             -- effectively:
                             --   mannerLexes = [(\m -> eval (open m :: l S)) | l <- refineBase ...]
                   return $ ds ++ [ls]

  where lexes  = refineMannerBase mannerUniv
                 -- all refinements of Base under the mannerUniv model
        ldecs  = foldr genLex (return ([],[])) lexes
                 -- for each refinement in lexes, declare evaluation behavior,
                 -- and return a Lexicon object reifying the evaluation function
        genLex = \mannerdict accum ->
          do (decs, evs) <- accum
             sn          <- show <$> newName "Manner"
             n           <- return (mkName sn)
             newdecs     <- deriveMannerLex n mannerdict
             let ev      = [| Lexicon sn       (\(MannerMessage m) -> eval (m :: $(conT n) S)) |]
                           -- Lexicon "Manner_123" (\m -> eval (open m :: Manner_123    S))
             return (decs ++ newdecs, evs ++ [ev])
