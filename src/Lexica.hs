{-# LANGUAGE TemplateHaskell #-}

module Lexica where

import Language.Haskell.TH
import Vocab


-- convenience type differentiating and labeling lexica
------------------------------------------------------------------------------
data Lexicon m = Lexicon
  { lexName :: String, interpret :: m -> Prop }
instance Eq (Lexicon m) where
  (Lexicon name _) == (Lexicon name' _) = name == name'
instance Ord (Lexicon m) where
  compare (Lexicon name _) (Lexicon name' _) = compare name name'
instance Show (Lexicon m) where
  show (Lexicon name _) = name

-- declare a lexicon type called `name`
genData :: Name -> Q Dec
genData name = dataD (cxt []) name    vars Nothing   fields             derives
            -- data           LexName a            = LexName (TypeOf a)
  where a       = mkName "a"
        vars    = [PlainTV a]
        b       = bang noSourceUnpackedness noSourceStrictness
        fields  = [normalC name [bangType b [t| TypeOf $(varT a) |]]]
        derives = []
