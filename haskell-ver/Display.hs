module Display where

import           Ast
import           Prelude hiding (GT, LT)

todo = error "todo"

class Display a where
  displays :: a -> [String]
  display :: a -> String

data DynDisplay =
  forall a. Display a =>
            Dis a

instance Display DynDisplay where
  display (Dis x) = display x
  displays (Dis x) = displays x

instance Display String where
  display = id
  displays x = [x]

instance Display Char where
  display c = [c]
  displays c = [[c]]

instance Display a => Display (Maybe a) where
  display = unwrap . fmap display
  displays = unwraps . fmap displays

unwrap m = case m of
  (Just x) -> x
  Nothing  -> "_"

unwraps m = case m of
  (Just x) -> x
  Nothing  -> ["_"]

orBlank :: Display a => Maybe a -> String
orBlank = unwrap . fmap display

join :: String -> [String] -> String
join c ss = case ss of
  []     -> ""
  [x]    -> x
  (x:xs) -> x ++ c ++ join c xs

indent :: String -> String
-- indent s = '\t' : s
-- indent s = "  " ++ s
indent s = "    " ++ s

indent' :: [String] -> [String]
indent' = map indent

noSeparation :: Display a => [a] -> [String]
noSeparation = map indent . concatMap displays

commaSeparationIndent :: (Display a, Display [a]) => [a] -> [String]
commaSeparationIndent xs = case xs of
  []       -> []
  [x]      -> [indent . display $ x]
  (x:y:xs) -> (indent (display x) ++ ",") : displays (y : xs)

commaSeparation :: (Display a, Display [a]) => [a] -> [String]
commaSeparation xs = case xs of
  []       -> []
  [x]      -> displays x
  (x:y:xs) -> (display x ++ ",") : displays (y : xs)

instance Display [Argument] where
  display = unlines . displays
  displays = commaSeparationIndent

instance Display [Decl] where
  display = unlines . displays
  displays = noSeparation

instance Display [Statement] where
  display = unlines . displays
  displays = noSeparation

instance Display [Maybe Expr] where
  display = unlines . displays
  displays = commaSeparation

instance Display [Expr] where
  display = unlines . displays
  displays = commaSeparation

instance Display Argument where
  display (Argument name ty) =
    let n = unwrap name
        t = unwrap ty
     in n ++ ": " ++ t
  displays a = [display a]

instance Display Bop where
  display b =
    case b of
      LT     -> "<"
      LTE    -> "<="
      GT     -> ">"
      GTE    -> ">="
      Eqish  -> "=="
      Eq     -> "==="
      Neqish -> "!="
      NEq    -> "!=="
      Add    -> "+"
      Sub    -> "-"
      Mul    -> "*"
      Div    -> "/"
      Mod    -> "%"
      And    -> "&&"
      Or     -> "||"
      Xor    -> "^"
      BOr    -> "|"
      BAnd   -> "&"
  displays b = [display b]

instance Display Uop where
  display u =
    case u of
      Not  -> "!"
      BNot -> "~"
  displays u = [display u]

instance Display Expr where
  display = unwords . displays
  displays e =
    case e of
      BinOps e [] -> [orBlank e]
      BinOps h ((op, e):es) ->
        let l = orBlank h
            o = orBlank op
            r = display (BinOps e es)
         in [l ++ ' ' : o ++ ' ' : r]
      UnOp o e -> [orBlank o ++ orBlank e]
      Ident s -> [s]
      Number n -> [n]
      Boolean True -> ["true"]
      Boolean False -> ["false"]
      Call f as ->
        let name = orBlank f
            args = join ", " . map orBlank $ as
         in [name ++ "(" ++ args ++ ")"]
      Object _ -> todo
      Function as r ss ->
        let args = join ", " . map display $ as
            body = map (indent . display) ss
            ret = unwrap r
            first = "function (" ++ args ++ "): " ++ ret ++ " {"
         in first : body ++ [['}']]

instance Display Statement where
  display = unwords . displays
  displays s =
    case s of
      SExpr e -> [orBlank e ++ ";"]
      Return Nothing -> ["return;"]
      Return (Just e) ->
        case displays e of
          [] -> ["return;"]
          [x] -> ["return " ++ x ++ ";"]
          (x:xs) ->
            let (l:ls) = reverse xs
             in todo
      If c t Nothing ->
        let cond = orBlank c
            then' = displays t
         in ("if (" ++ cond ++ ") {") : then' ++ ["}"]
      If c t (Just e) ->
        let cond = orBlank c
            then' = displays t
            else' = displays e
         in ("if (" ++ cond ++ ") {") : then' ++ ["} else {"] ++ else' ++ ["}"]
      SDecl d -> map (++ ";") . displays $ d

instance Display Decl where
  display = unwords . displays
  displays d =
    case d of
      Let n t e ->
        let name = unwrap n
            ty = unwrap t
            expr = orBlank e
         in ["let " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Const n t e ->
        let name = unwrap n
            ty = unwrap t
            expr = orBlank e
         in ["const " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Func n as r ss ->
        let name = unwrap n
            args = displays as
            ret = unwrap r
            expr = displays ss
         in ("function " ++ name ++ "(") :
            args ++ ("): " ++ ret ++ " {") : expr ++ ["}"]
