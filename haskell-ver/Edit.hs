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
