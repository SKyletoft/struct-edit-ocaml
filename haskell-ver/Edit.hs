module Edit where

import           Ast

todo = error "Not yet implemented"

data Action
  = EditText String
  | InsertStatement
  | AppendStatement
  | InsertExpression

class Navigate a where
  visit :: a -> Int -> Maybe DynNavigate

data DynNavigate =
  forall a. Navigate a =>
            Nav a

instance Navigate DynNavigate where
  visit (Nav x) = visit x

instance Navigate (Maybe String) where
  visit _ _ = Nothing

instance Navigate Decl where
  visit _ _ = todo

instance Navigate Argument where
  visit _ _ = todo

instance Navigate Expr where
  visit _ _ = todo

instance Navigate Statement where
  visit _ _ = todo
