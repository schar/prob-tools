{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Experiments.Scalar.RefinementsForDays.Lexica where

import Vocab
import Experiments.Scalar.RefinementsForDays.Domain
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List (intersect, nub, subsequences, (\\))


-- Scalar lexica  interpret terms as familiar e/s/t denotations
------------------------------------------------------------------------------
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
newtype GQMessage = GQMessage (forall f. (Grammar f, NameLex f, SALex f, GQLex f) => f S)
mkMessageInstances ''GQMessage 'GQMessage

-- base lexicon
------------------------------------------------------------------------------
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

baseGQLex :: Lexicon GQMessage World
baseGQLex = Lexicon "Base" (\(GQMessage m) -> runBase m)

-- macros to generate lexicon instances for GQ refinements of Base
------------------------------------------------------------------------------

data GQDict = GQDict
  { somePlayer'  :: [[Entity]]
  , everyPlayer' :: [[Entity]]
  }
  deriving (Eq, Show, Lift)

{--}
refineGQBase :: [Entity] -> [World] -> [GQDict]
refineGQBase dom univ =
  let pset    = tail . subsequences
                -- powerset-plus function
      propDom = pset dom
      pSome   = pset [q | q <- propDom, runBase (somePlayer  (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
      pEvery  = pset [q | q <- propDom, runBase (everyPlayer (\(B x) -> B $ \_ -> x `elem` q)) (head univ)]
   in [ GQDict {somePlayer' = sm, everyPlayer' = ev} | sm <- pSome, ev <- pEvery ]
--}

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
