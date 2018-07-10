{-# LANGUAGE TemplateHaskell #-}

module Lexica where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import Vocab


-- convenience type differentiating and labeling lexica
------------------------------------------------------------------------------
data Lexicon m w = Lexicon
  { lexName :: String, interpret :: m -> w -> Bool }
instance Eq (Lexicon m w) where
  (Lexicon name _) == (Lexicon name' _) = name == name'
instance Ord (Lexicon m w) where
  compare (Lexicon name _) (Lexicon name' _) = compare name name'
instance Show (Lexicon m w) where
  show (Lexicon name _) = name

mkMessageInstances :: Name -> Q [Dec]
mkMessageInstances name =
  [d| instance Eq $t where
        --  ( $d m ) == ( $d m' ) = (m :: ParseTree S) == m'
      -- instance Ord GQMessage where
      --   compare (GQMessage m) (GQMessage m') = compare (m :: ParseTree S) m'
      -- instance Show GQMessage where
      --   show (GQMessage m) = show (m :: ParseTree S)
  |]
  where t = conT name
        d = conE name
