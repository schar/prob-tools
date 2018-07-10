{-# LANGUAGE TemplateHaskell #-}

module Lexica where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import Vocab


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
