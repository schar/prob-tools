{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Lexica.Manner where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lexica
import Lexica.Base
import Vocab
import Model
import Data.Tree
import Data.Functor.Classes (compare1)
import Data.List            (subsequences, intercalate, nub, intersect)

-- The Manner lexicon interprets terms as threshold-dependent e/s/t denotations
------------------------------------------------------------------------------

data MannerDict = MannerDict
  { started' :: [World]
  , gotStarted' :: [World]
  }
  deriving (Eq, Show, Lift)

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
