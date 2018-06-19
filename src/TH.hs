{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import LUM
import Lexica
import Model
import Control.Monad (MonadPlus, forM, filterM)
import Data.List ((\\))


-- macros to generate lexicon instances for refinements of Base
------------------------------------------------------------------------------

-- characteristic sets representing an SA refinement, given a concrete model
data SADict = SADict
  { scored' :: [(Entity,World)]
  , aced'   :: [(Entity,World)]
  }
  deriving (Eq, Show, Lift)

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
                s $px $pf   = $d (f x)
                tvp $pf $px = $d (f x)
                nil         = $d (const True)

              instance Eval $t where
                eval ($px)  = x

              instance NameLex $t where
                john        = $d John
                mary        = $d Mary

              instance SALex $t where
                scored      = $d (\x w -> (x,w) `elem` scored' sadict)
                aced        = $d (\x w -> (x,w) `elem` aced' sadict)
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]

-- at a concrete model with domain of entities `dom` and domain of worlds `univ`,
-- generate all possible refinements of the Base lexicon
refineSABase :: [Entity] -> [World] -> [SADict]
refineSABase dom univ =
  let pset    = (\\ [[]]) . filterM (const [True,False])
                -- powerset-plus function
      pScored = pset [(x,w) | x <- dom, w <- univ, runBase scored x w]
                -- powerset of Base's interpretation of "scored"
      pAced   = pset [(x,w) | x <- dom, w <- univ, runBase aced x w]
                -- powerset of Base's interpretation of "aced"
   in [ SADict {scored' = sc, aced' = ac} | sc <- pScored, ac <- pAced ]

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
             let ev      = [| Lexicon sn       (\m -> eval (open m :: $(conT n) S)) |]
                           -- Lexicon "SA_123" (\m -> eval (open m :: SA_123    S))
             return (decs ++ newdecs, evs ++ [ev])

