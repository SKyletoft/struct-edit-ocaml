{-# LANGUAGE UndecidableInstances #-}

module Highlight where

import           GHC.Stack
import           Debug.Trace

import           Ast
import           Display   hiding (todo)
import           Prelude   hiding (GT, LT, (!!))

infixl 9 !!

(!!) :: HasCallStack => [a] -> Int -> a
[] !! _     = error "(!!): index out of scope!"
(x:_) !! 0  = x
(_:xs) !! n = xs !! (n - 1)

type Cursor = [Int]

invertL = "\ESC[7m"
resetL  = "\ESC[0m"

invert :: String -> String
invert s = invertL ++ s ++ resetL

idisplay :: Display a => a -> String
idisplay = invert . display

idisplays :: Display a => a -> [String]
idisplays = map invert . displays

invertLine :: String -> String
invertLine s =
  let isWhitespace c = c == ' ' || c == '\t'
      trim = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
      indent = takeWhile isWhitespace s
      content = trim s
   in indent ++ invert content

todo :: HasCallStack => a
todo = error "todo"

highlights' ::
    HasCallStack => Highlight a =>
       [Int] -> a -> [String]
highlights' []  = displays
highlights' [0] = map invert . displays
highlights' is  = highlights is

highlight' ::
    HasCallStack => Highlight a =>
       [Int] -> a -> String
highlight' []  = display
highlight' [0] = invert . display
highlight' is  = highlight is

highlightOrDisplay ::
    HasCallStack => Highlight a =>
       [Int] -> Int -> Int -> a -> String
highlightOrDisplay is i' i e
  | i == i' = highlight' is e
  | otherwise = display e

highlightsOrDisplays ::
    HasCallStack => Highlight a =>
       [Int] -> Int -> Int -> a -> [String]
highlightsOrDisplays is i' i e
  | i == i' = highlights' is e
  | otherwise = displays e

splitUp :: HasCallStack => [a] -> Int -> ([a], a, [a])
splitUp xs i =
  let pre = take i xs
      x = xs !! i
      post = drop (i + 1) xs
   in (pre, x, post)

class Display a =>
      Highlight a
  where
  highlights :: HasCallStack => [Int] -> a -> [String]
  highlight :: HasCallStack => [Int] -> a -> String

data DynHighlight =
  forall a. Highlight a =>
            Hi a

instance Display DynHighlight where
  display (Hi x) = display x
  displays (Hi x) = displays x

instance Highlight DynHighlight where
  highlight is (Hi x) = highlight is x
  highlights is (Hi x) = highlights is x

instance Highlight Char where
  highlight is = error $ "Invalid cursor digging into Char: " ++ show is
  highlights is = error $ "Invalid cursor digging into Char: " ++ show is

instance Highlight a => Highlight (Maybe a) where
  highlight (0:is) = unwrap . fmap (highlight' is)
  highlight is = error $ "Invalid cursor digging into Maybe: " ++ show is
  highlights (0:is) = unwraps . fmap (highlights' is)
  highlights is = error $ "Invalid cursor digging into Maybe: " ++ show is

instance (Display [a], Highlight a) => Highlight [a] where
  highlight (i:is) ds =
    let (pre, x, post) = splitUp ds i
        pre' = display pre
        post' = display post
        x' = highlight' is x
     in pre' ++ x' ++ post'
  highlights (i:is) ds =
    let (pre, x, post) = splitUp ds i
        pre' = displays pre
        post' = displays post
        x' = map indent . highlights' is $ x
     in pre' ++ x' ++ post'

instance Highlight Argument where
  highlight (i:is) (Argument name ty) =
    let n = highlightOrDisplay is 0 i name
        t = highlightOrDisplay is 1 i ty
     in n ++ ": " ++ t
  highlights is a = [highlight is a]

instance Highlight Bop where
  highlight = todo
  highlights = todo

instance Highlight Uop where
  highlight = todo
  highlights = todo

instance Highlight Expr where
  highlight is = unwords . highlights is
  highlights (i:is) e = case e of
    BinOps e [] | i == 0 -> highlights' is e
    BinOps h ((op, e):es) ->
      let l = highlightOrDisplay is i 0 h
          o = highlightOrDisplay is i 1 op
          r | i >= 2    = highlight' ((i - 2):is) (BinOps e es)
            | otherwise = display (BinOps e es)
       in [l ++ ' ' : o ++ ' ' : r]
    UnOp mu me ->
      let mu' = highlightOrDisplay is i 0 mu
          me' = highlightOrDisplay is i 1 me
       in [mu' ++ me']
    Ident i     -> highlights' is i
    Number n    -> highlights' is n
    Boolean b   -> highlights' is (display e)
    Call me mes ->
      let me' = highlightOrDisplay is i 0 me
          mes' = highlightOrDisplay is i 1 mes
       in [me' ++ "(" ++ mes' ++ ")"]
    Object msmes      -> todo
    Function as ms ss -> todo
    Lambda as ss      -> todo

instance Highlight Statement where
  highlight = todo
  highlights (i:is) s@(If {}) = case s of
      If c t Nothing ->
        let cond  = highlightOrDisplay   is i 0 c
            then' = highlightsOrDisplays is i 1 t
         in ("if (" ++ cond ++ ") {")
            : then' ++ ["}"]
      If c t (Just e) ->
        let cond  = highlightOrDisplay   is i 0 c
            then' = highlightsOrDisplays is i 1 t
            else' = highlightsOrDisplays is i 2 e
         in ("if (" ++ cond ++ ") {")
            : then' ++ ["} else {"] ++ else' ++ ["}"]
  highlights (0:is) s = case s of
    SExpr me  -> highlights' is me
    Return me -> highlights' is me
    SDecl d   -> highlights' is d
  highlights is s = error $ show (is, s)

instance Highlight Decl where
  highlight = todo
  highlights (i:is) d =
    case d of
      Let n t e ->
        let name = highlightOrDisplay is i 0 n
            ty   = highlightOrDisplay is i 1 t
            expr = highlightOrDisplay is i 2 e
         in ["let " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Const n t e ->
        let name = highlightOrDisplay is i 0 n
            ty   = highlightOrDisplay is i 1 t
            expr = highlightOrDisplay is i 2 e
         in ["const " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Func n as r ss ->
        let name = highlightOrDisplay   is i 0 n
            args = highlightsOrDisplays is i 1 as
            ret  = highlightOrDisplay   is i 2 r
            expr = highlightsOrDisplays is i 3 ss
         in ("function " ++ name ++ "(")
            : args ++ ("): " ++ ret ++ " {") : expr ++ ["}"]
