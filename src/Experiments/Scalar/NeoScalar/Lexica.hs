{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Experiments.Scalar.NeoScalar.Lexica where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import LUM
-- import Lexica (genData)
import Vocab
import Lexica.ParseTree
import Lexica
import Experiments.Scalar.NeoScalar.Domain
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

newtype GQMessage = GQMessage (forall f. (Grammar f, NameLex f, SALex f, GQLex f) => f S)
instance Eq GQMessage where
  (GQMessage m) == (GQMessage m') = (m :: ParseTree S) == m'
instance Ord GQMessage where
  compare (GQMessage m) (GQMessage m') = compare (m :: ParseTree S) m'
instance Show GQMessage where
  show (GQMessage m) = show (m :: ParseTree S)

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

baseGQLex :: Lexicon GQMessage World
baseGQLex = Lexicon "Base" (\(GQMessage m) -> runBase m)

-- macros to generate lexicon instances for GQ refinements of Base
------------------------------------------------------------------------------

data NeoDict = NeoDict
  { johnQ' :: Q Exp
  , maryQ' :: Q Exp
  , scored' :: Q Exp
  , aced' :: Q Exp
  , noPlayer' :: Q Exp
  , somePlayer'  :: Q Exp
  , everyPlayer' :: Q Exp
  }
  -- deriving (Eq, Show, Lift)

refineNeoBase :: [Entity] -> [NeoDict]
refineNeoBase dom =
  let pJohn = [ [| \q -> q John |]
              , [| \q w -> q John w && all (\x -> not (q x w)) (dom \\ [John]) |]
              ]
      pMary = [ [| \q -> q Mary |]
              , [| \q w -> q Mary w && all (\x -> not (q x w)) (dom \\ [Mary]) |]
              ]
      pNo   = [ [| \q -> runBase (noPlayer (\(B x) -> B $ q x)) |] ]
      pScored = [ [| runBase scored |]
                , [| \w -> runBase scored w \\ runBase aced w |]
                ]
      pAced   = [ [| runBase aced |] ]
      pSome  = [ [| \q -> runBase (somePlayer (\(B x) -> B $ q x)) |]
               , [| \q w -> runBase (somePlayer (\(B x) -> B $ q x)) w
                            && not (runBase (everyPlayer (\(B x) -> B $ q x)) w) |]
               ]
      pEvery = [ [| \q -> runBase (everyPlayer (\(B x) -> B $ q x)) |] ]
   in [ NeoDict
        { johnQ' = jn
        , maryQ' = mr
        , scored' = sc
        , aced' = ac
        , noPlayer' = no
        , somePlayer' = sm
        , everyPlayer' = ev
        }
      | jn <- pJohn, mr <- pMary, no <- pNo, sc <- pScored, ac <- pAced, sm <- pSome, ev <- pEvery
      ]


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
deriveNeoLex :: Name -> NeoDict -> Q [Dec]
deriveNeoLex name neodict = do
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
                scored      = $d $(scored' neodict)
                aced        = $d $(aced' neodict)

              instance GQLex $t where
                johnQ q  = $d ($(johnQ' neodict) (\x -> case (q ($d x)) of {$px -> x}))
                maryQ q  = $d ($(maryQ' neodict) (\x -> eval (q ($d x))))
                noPlayer q = $d ($(noPlayer' neodict) (\x -> eval (q ($d x))))
                somePlayer q  = $d ($(somePlayer' neodict) (\x -> eval (q ($d x))))
                everyPlayer q = $d ($(everyPlayer' neodict) (\x -> eval (q ($d x))))
          |]
  return $ ddec : idec
  where t  = conT name
        d  = conE name
        px = conP name [[p|x|]]
        pf = conP name [[p|f|]]

-- declare lexica for all possible refinements of Base
mkNeoLexes :: Q [Dec]
mkNeoLexes = do ds <- traverse fst ldecs
                ls <- valD (varP (mkName "neoLexes")) (normalB $ listE $ fmap snd ldecs) []
                         -- effectively:
                         --   saLexes = [(\m -> eval (open m :: l S)) | l <- refineBase ...]
                return $ concat ds ++ [ls]

  where neodicts = refineNeoBase gqDom
                  -- all neoGricean refinements of Base
        ldecs   = zipWith genLex neodicts [1..]
        genLex neodict n =
          let strname = "Neo" ++ show n
              name    = mkName strname
           in ( deriveNeoLex name neodict
              , [| Lexicon strname (\(GQMessage m) -> case (m :: $(conT name) S) of {$(conP name [[p|p|]]) -> p}) |]
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
