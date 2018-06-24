{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Lexica.GQ where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import LUM
-- import Lexica (genData)
import Vocab
import Lexica.Base
import Lexica.ParseTree
import Lexica
import Model
import Control.Monad (MonadPlus, forM, filterM)
import Data.List (subsequences, (\\))


-- macros to generate lexicon instances for GQ refinements of Base
------------------------------------------------------------------------------

data GQDict = GQDict
  { somePlayer'  :: [[Entity]]
  , everyPlayer' :: [[Entity]]
  }
  deriving (Eq, Show, Lift)

-- given a refinement dictionary, declare the evaluation algebras that define
-- the `name` lexicon
deriveGQLex :: Name -> GQDict -> Q [Dec]
deriveGQLex name gqdict = do
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
                scored      = $d (runBase scored)
                aced        = $d (runBase aced)

              instance GQLex $t where
                johnQ q = $d (runBase (johnQ (\(B x) -> B $ eval (q ($d x)))))
                maryQ q = $d (runBase (maryQ (\(B x) -> B $ eval (q ($d x)))))
                noPlayer q = $d (runBase (noPlayer (\(B x) -> B $ eval (q ($d x)))))
                somePlayer q  = $d (\w -> [x | x <- gqDom, eval (q ($d x)) w] `elem` somePlayer'  gqdict)
                everyPlayer q = $d (\w -> [x | x <- gqDom, eval (q ($d x)) w] `elem` everyPlayer' gqdict)
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]


refineGQBase :: [Entity] -> [World] -> [GQDict]
-- takes about an hour to compile, due to ~64K refinements
{--}
refineGQBase dom univ =
  let pset    = tail . subsequences
                -- powerset-plus function
      propDom = pset dom
      pSome   = pset [q | q <- propDom, runBase (somePlayer  (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
      pEvery  = pset [q | q <- propDom, runBase (everyPlayer (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
   in [ GQDict {somePlayer' = sm, everyPlayer' = ev} | sm <- pSome, ev <- pEvery ]
--}

-- declare lexica for all possible refinements of Base
mkGQLexes :: Q [Dec]
mkGQLexes = do ds <- traverse fst ldecs
               ls <- valD (varP (mkName "gqLexes")) (normalB $ listE $ fmap snd ldecs) []
                        -- effectively:
                        --   saLexes = [(\m -> eval (open m :: l S)) | l <- refineBase ...]
               return $ concat ds ++ [ls]

  where gqdicts = refineGQBase gqDom gqUniv
                  -- all refinements of Base under the saDom, saUniv model
        ldecs   = zipWith genLex gqdicts [1..]
        genLex gqdict n =
          let strname = "GQ" ++ show n
              name    = mkName strname
           in ( deriveGQLex name gqdict
              , [| Lexicon strname (\(GQMessage m) -> eval (m :: $(conT name) S)) |]
              )
        {--
        ldecs  = foldr genLex (return ([],[])) lexes
                 -- for each refinement in lexes, declare evaluation behavior,
                 -- and return a Lexicon object reifying the evaluation function
        genLex = \gqdict accum ->
          do (decs, evs) <- accum
             sn          <- show <$> newName "GQ"
             n           <- return (mkName sn)
             newdecs     <- deriveGQLex n gqdict
             let ev      = [| Lexicon sn       (\m -> eval (open m :: $(conT n) S)) |]
                           -- Lexicon "GQ_123" (\m -> eval (open m :: GQ_123    S))
             return (decs ++ newdecs, evs ++ [ev])
        --}
