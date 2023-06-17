module Navigate where

import           Debug.Trace
import           GHC.Stack
import           Prelude     hiding ((!!), Left, Right)

import           Ast
import           Edit (Edit)

type Cursor = [Int]

data Direction
  = In
  | Out
  | Left
  | Right

data Res
  = New Cursor
  | Old Cursor

infixl 9 !!

(!!) :: HasCallStack => [a] -> Int -> a
[] !! _     = error "(!!): index out of scope!"
(x:_) !! 0  = x
(_:xs) !! n = xs !! (n - 1)

unwrap :: Res -> Cursor
unwrap c = case c of
  New x -> x
  Old x -> x

navigate :: Navigate a => a -> Cursor -> Direction -> Cursor
navigate n cur d = case d of
  In -> cur ++ [0]
  Out -> init cur
  Left ->
    let (c:cs) = reverse cur
     in reverse (c - 1 : cs)
  Right ->
    let (c:cs) = reverse cur
     in reverse (c + 1 : cs)

todo :: HasCallStack => a
todo = error "Not yet implemented"

class Navigate a where
  maxCursor :: HasCallStack => a -> Int
  validate :: HasCallStack => a -> [Int] -> Bool

data DynNav =
  forall a. (Navigate a) =>
            Nav a

instance Navigate DynNav where
  maxCursor (Nav x) = maxCursor x
  validate (Nav x) = validate x

instance Navigate Char where
  maxCursor is = error $ "Invalid cursor digging into Char: " ++ show is
  validate _ _ = todo

instance Navigate a => Navigate (Maybe a) where
  maxCursor _ = 1
  validate _ _ = todo

instance Navigate a => Navigate [a] where
  maxCursor = length
  validate _ _ = todo

instance Navigate Argument where
  maxCursor _ = 2
  validate _ _ = todo

instance Navigate Bop where
  maxCursor _ = 1
  validate _ _ = todo

instance Navigate Uop where
  maxCursor _ = 1
  validate _ _ = todo

instance Navigate Expr where
  maxCursor e = case e of
    BinOps _ es -> length es * 2
    UnOp _ _    -> 2
    Ident _     -> 1
    Number _    -> 1
    Boolean _   -> 1
    Call _ _    -> 2
    Object es   -> length es
    Function {} -> 3
    Lambda _ _  -> 2
  validate _ _ = todo

instance Navigate Statement where
  maxCursor s = case s of
    SExpr {} -> 1
    Return _ -> 1
    If {}    -> 3
    SDecl _  -> 1
  validate _ _ = todo

instance Navigate Decl where
  maxCursor d = case d of
    Func {} -> 4
    _       -> 3
  validate _ _ = todo

