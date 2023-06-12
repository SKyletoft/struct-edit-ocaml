{-# LANGUAGE UndecidableInstances #-}

module Highlight where

import           GHC.Stack
import           Debug.Trace

import           Ast
import           Display   hiding (todo)
import           Prelude   hiding (GT, LT)

invertL = "\ESC[7m"

resetL = "\ESC[0m"

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
    Highlight a =>
       [Int] -> a -> [String]
highlights' []  = displays
highlights' [0] = map invert . displays
highlights' is  = highlights is

highlight' ::
    Highlight a =>
       [Int] -> a -> String
highlight' []  = display
highlight' [0] = invert . display
highlight' is  = highlight is

highlightOrDisplay i' i is e
  | i == i' = highlight' is e
  | otherwise = display e

highlightsOrDisplays i' i is e
  | i == i' = highlights' is e
  | otherwise = displays e

splitUp :: [a] -> Int -> ([a], a, [a])
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
  highlight is = todo
  highlights is x = todo

instance Highlight a => Highlight (Maybe a) where
  highlight (0:is) = highlight' is
  highlights (0:is) = highlights' is

instance (Display [a], Highlight a) => Highlight [a] where
  highlight is = todo
  highlights (i:is) ds =
    let (pre, x, post) = splitUp ds i
        pre' = displays pre
        post' = displays post
        x' = map indent . highlights' is $ x
     in pre' ++ x' ++ post'

instance Highlight Argument where
  highlight is (Argument name ty) = todo
  highlights is a = todo

instance Highlight Bop where
  highlight = todo
  highlights = todo

instance Highlight Uop where
  highlight = todo
  highlights = todo

instance Highlight Expr where
  highlight = todo
  highlights = todo

instance Highlight Statement where
  highlight = todo
  highlights (i:is) s@(If {}) = case s of
      If c t Nothing ->
        let cond  = highlightOrDisplay   0 i is c
            then' = highlightsOrDisplays 1 i is t
         in ("if (" ++ cond ++ ") {") : then' ++ ["}"]
      If c t (Just e) ->
        let cond  = highlightOrDisplay   0 i is c
            then' = highlightsOrDisplays 1 i is t
            else' = highlightsOrDisplays 2 i is e
         in ("if (" ++ cond ++ ") {") : then' ++ ["} else {"] ++ else' ++ ["}"]
  highlights (0:is) s = case s of
    SExpr me  -> highlights' is me
    Return me -> highlights' is me
    SDecl d   -> highlights' is d

instance Highlight Decl where
  highlight = todo
  highlights (i:is) d =
    case d of
      Let n t e ->
        let name = highlightOrDisplay 0 i is n
            ty   = highlightOrDisplay 1 i is t
            expr = highlightOrDisplay 2 i is e
         in ["let " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Const n t e ->
        let name = highlightOrDisplay 0 i is n
            ty   = highlightOrDisplay 1 i is t
            expr = highlightOrDisplay 2 i is e
         in ["const " ++ name ++ ": " ++ ty ++ " = " ++ expr]
      Func n as r ss ->
        let name = highlightOrDisplay   0 i is n
            args = highlightsOrDisplays 1 i is as
            ret  = highlightOrDisplay   2 i is r
            expr = highlightsOrDisplays 3 i is ss
         in ("function " ++ name ++ "(") :
            args ++ ("): " ++ ret ++ " {") : expr ++ ["}"]
