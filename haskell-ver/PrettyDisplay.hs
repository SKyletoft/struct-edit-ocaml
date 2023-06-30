module PrettyDisplay where

import Text.PrettyPrint
import Prelude hiding ((<>))

import Ast

class PrettyDisplay a where
  prettyDisplay :: a -> Doc


whileLoop
  = text "while "
   <> parens (text "x < 5")
   <> braces (empty $$ text "x++;")
