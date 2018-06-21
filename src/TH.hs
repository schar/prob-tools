{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import LUM
import Lexica
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
                someShot  q   = $d $ \w -> any (\y -> eval (q ($d y)) w) (shot' w)
                everyShot q   = $d $ \w -> all (\y -> eval (q ($d y)) w) (shot' w)
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
             let ev      = [| Lexicon sn       (\m -> eval (open m :: $(conT n) S)) |]
                           -- Lexicon "SA_123" (\m -> eval (open m :: SA_123    S))
             return (decs ++ newdecs, evs ++ [ev])


-- macros to generate lexicon instances for GQ refinements of Base
------------------------------------------------------------------------------

data GQDict = GQDict
  { someShot'  :: [[Entity]]
  , everyShot' :: [[Entity]]
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
                someShot  q = $d (\w -> [x | x <- gqDom, eval (q ($d x)) w] `elem` someShot'  gqdict)
                everyShot q = $d (\w -> [x | x <- gqDom, eval (q ($d x)) w] `elem` everyShot' gqdict)
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]


refineGQBase :: [Entity] -> [World] -> [GQDict]
-- can't be compiled due to practically infinitely many refinements!
{--}
refineGQBase dom univ =
  let pset    = tail . subsequences
                -- powerset-plus function
      propDom = pset dom
      pSome   = pset [q | q <- propDom, runBase (someShot  (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
      pEvery  = pset [q | q <- propDom, runBase (everyShot (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
   in [ GQDict {someShot' = sm, everyShot' = ev} | sm <- pSome, ev <- pEvery ]
--}
{--
refineGQBase dom univ =
  let pset    :: Eq a => [a] -> [[a]]
      pset    = tail . subsequences
                -- powerset-plus function
      propDom = pset dom
      pSome  = [ [(q,w) | q <- propDom, w <- univ, runBase (someShot  (\(B x) -> B $ \w' -> (x,w') `elem` q)) w]
               , [(q,w) | q <- propDom, w <- univ,       runBase (someShot  (\(B x) -> B $ \w' -> (x,w') `elem` q)) w
                                                 , not $ runBase (everyShot (\(B x) -> B $ \w' -> (x,w') `elem` q)) w]
               ]
      pEvery = [ [(q,w) | q <- propDom, w <- univ, runBase (everyShot (\(B x) -> B $ \w' -> (x,w') `elem` q)) w]
               ]
  in [ GQDict {someShot' = sm, everyShot' = ev} | sm <- pSome, ev <- pEvery ]
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
              , [| Lexicon strname (\m -> eval (open m :: $(conT name) S)) |]
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
mkLexes :: String -> Q [Dec]
mkLexes s = case s of {"SA" -> mkSALexes; "GQ" -> mkGQLexes}
