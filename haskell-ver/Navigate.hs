module Navigate where

import           Debug.Trace
import           GHC.Stack
import           Prelude     hiding (Left, Right, (!!))

import           Ast
import           Edit        (Edit)

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
(x:xs) !! n
  | n == 0 = x
  | otherwise = xs !! (n - 1)

todo :: HasCallStack => a
todo = error "Not yet implemented"

unwrap :: Res -> Cursor
unwrap c =
  case c of
    New x -> x
    Old x -> x

navigate :: Navigate a => a -> Direction -> Cursor -> Cursor
navigate _ _ [] = []
navigate n d cur =
  let retry d' = navigate n In . navigate n d' . navigate n Out
      up d' = navigate n d' . navigate n Out
      (c:cs) = reverse cur
      try c'
        | validate n c' = c'
        | otherwise = cur
   in case d of
        In ->
          let candidate = cur ++ [0]
           in try candidate
        Out ->
          let candidate = init cur
           in try candidate
        Left ->
          let candidate = reverse (c - 1 : cs)
              retry' = retry Left cur
              up' = up Left cur
              ret
                | validate n candidate = candidate
                | validate n retry' = retry'
                | validate n up' = up'
                | otherwise = cur
           in ret
        Right ->
          let candidate = reverse (c + 1 : cs)
              retry' = retry Right cur
              up' = up Right cur
              ret
                | validate n candidate = candidate
                | validate n retry' = retry'
                | validate n up' = up'
                | otherwise = cur
           in ret

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
  validate _ is =
    case is of
      []  -> True
      [0] -> True
      _   -> False

instance Navigate a => Navigate (Maybe a) where
  maxCursor _ = 1
  validate m is =
    case m of
      Just x ->
        case is of
          []     -> True
          (0:is) -> validate x is
          _      -> False
      Nothing ->
        case is of
          [0] -> True
          _   -> False

instance Navigate a => Navigate [a] where
  maxCursor = length
  validate x is =
    case is of
      []     -> True
      (i:is) -> i >= 0 && i < length x && validate (x !! i) is

instance Navigate Argument where
  maxCursor _ = 2
  validate (Argument n t) = validate (n, t)

instance Navigate Bop where
  maxCursor _ = 1
  validate _ is =
    case is of
      []  -> True
      [0] -> True
      _   -> False

instance Navigate Uop where
  maxCursor _ = 1
  validate _ is =
    case is of
      []  -> True
      [0] -> True
      _   -> False

instance Navigate Expr where
  maxCursor e =
    case e of
      BinOps _ es -> length es * 2
      UnOp _ _    -> 2
      Ident _     -> 1
      Number _    -> 1
      Boolean _   -> 1
      Call _ _    -> 2
      Object es   -> length es
      Function {} -> 3
      Lambda _ _  -> 2
  validate e is =
    case e of
      BinOps h es ->
        case is of
          [] -> True
          (0:is) -> validate h is
          (i:is)
            | i >= 0 && i < maxCursor e ->
              let i' = i `div` 2
                  (o, e') = es !! i'
                  o' = validate o is
                  e'' = validate e' is
               in if even i
                    then e''
                    else o'
          _ -> False
      UnOp o u ->
        case is of
          []     -> True
          (0:is) -> validate o is
          (1:is) -> validate u is
          _      -> False
      Ident n -> validate n is
      Number n -> validate n is
      Boolean _ ->
        case is of
          []  -> True
          [0] -> True
          _   -> False
      Call f as ->
        case is of
          []     -> True
          (0:is) -> validate f is
          (1:is) -> validate as is
          _      -> False
      Object es ->
        case is of
          []     -> True
          (i:is) -> i >= 0 && i < maxCursor e && validate es is
      Function as t ss ->
        case is of
          []     -> True
          (0:is) -> validate as is
          (1:is) -> validate t is
          (2:is) -> validate ss is
          _      -> False
      Lambda as ss ->
        case is of
          []     -> True
          (0:is) -> validate as is
          (1:is) -> validate ss is
          _      -> False

instance (Navigate a, Navigate b) => Navigate (a, b) where
  maxCursor _ = 2
  validate (n, t) is =
    case is of
      []     -> True
      (0:is) -> validate n is
      (1:is) -> validate t is
      _      -> False

instance Navigate Statement where
  maxCursor s =
    case s of
      SExpr {} -> 1
      Return _ -> 1
      If {}    -> 3
      SDecl _  -> 1
  validate s is =
    case s of
      SExpr e ->
        case is of
          []     -> True
          (0:is) -> validate e is
          _      -> False
      Return e ->
        case is of
          []     -> True
          (0:is) -> validate e is
          _      -> False
      If c t e ->
        case is of
          []     -> True
          (0:is) -> validate c is
          (1:is) -> validate t is
          (2:is) -> validate e is
          _      -> False
      SDecl d ->
        case is of
          []     -> True
          (0:is) -> validate d is
          _      -> False

instance Navigate Decl where
  maxCursor d =
    case d of
      Func {} -> 4
      _       -> 3
  validate d is =
    case d of
      Let n t e ->
        case is of
          []     -> True
          (0:is) -> validate n is
          (1:is) -> validate t is
          (2:is) -> validate e is
          _      -> False
      Const n t e ->
        case is of
          []     -> True
          (0:is) -> validate n is
          (1:is) -> validate t is
          (2:is) -> validate e is
          _      -> False
      Func n as t ss ->
        case is of
          []     -> True
          (0:is) -> validate n is
          (1:is) -> validate as is
          (2:is) -> validate t is
          (3:is) -> validate ss is
          _      -> False
