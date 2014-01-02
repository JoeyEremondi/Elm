module SourceSyntax.Literal where

{-# LANGUAGE DeriveDataTypeable #-}

import SourceSyntax.PrettyPrint
import qualified Text.PrettyPrint as PP
import Data.Data

data Literal = IntNum Int
             | FloatNum Double
             | Chr Char
             | Str String
             | Boolean Bool
               deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Literal where
  pretty literal =
    case literal of
      IntNum n -> PP.int n
      FloatNum n -> PP.double n
      Chr c -> PP.quotes (PP.char c)
      Str s -> PP.text (show s)
      Boolean bool -> PP.text (show bool)