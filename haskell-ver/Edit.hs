module Edit where

import           Ast
import           Display hiding (todo)

todo = error "Not yet implemented"

data Action
  = EditText String
  | InsertStatement
  | AppendStatement
  | InsertExpression

class Display a =>
      Navigate a
  where
  visit :: a -> Int -> Maybe DynNavigate

class Navigate a =>
      Edit a
  where
  edit :: a -> Int -> Action -> DynNavigate
  editInner :: a -> [Int] -> Action -> DynNavigate
  actions :: a -> [Action]

data DynNavigate =
  forall a. (Display a, Navigate a) =>
            Nav a

instance Display DynNavigate where
  display (Nav x) = display x
  displays (Nav x) = displays x

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

instance Edit Decl where
  actions _ = todo
  edit _ _ _ = error "Invalid action"
  editInner _ _ _ = todo

instance Edit Argument where
  actions _ = todo
  edit _ _ _ = error "Invalid action"
  editInner _ _ _ = todo

instance Edit Expr where
  actions _ = todo
  edit _ _ _ = error "Invalid action"
  editInner _ _ _ = todo

instance Edit Statement where
  actions _ = todo
  edit _ _ _ = error "Invalid action"
  editInner _ _ _ = todo
