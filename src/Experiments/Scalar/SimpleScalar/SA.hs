{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Lexica.SA where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import LUM
import Lexica
import Lexica.Base
import Vocab
import Model
import Control.Monad (MonadPlus, forM, filterM)
import Data.List (subsequences, (\\))


-- macros to generate lexicon instances for refinements of Base
------------------------------------------------------------------------------

-- characteristic sets representing an SA refinement, given a concrete model
data SADict = SADict
  { scored' :: [(Entity,World)]
  , aced'   :: [(Entity,World)]
  }
  deriving (Eq, Show, Lift)


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
