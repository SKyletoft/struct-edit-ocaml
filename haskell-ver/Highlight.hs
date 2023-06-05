module Highlight where

import           Ast
import           Prelude hiding (GT, LT)
import           Display hiding (todo)

invertL = "\ESC[7m"
resetL = "\ESC[0m"

invert :: String -> String
invert s = invertL ++ s ++ resetL

invertLine :: String -> String
invertLine s =
  let isWhitespace c = c == ' ' || c == '\t'
      indent = takeWhile isWhitespace s
      content = dropWhile isWhitespace s
   in indent ++ invert s

todo = error "todo"

highlights' :: Highlight.Highlight a => Highlight a => [Int] -> a -> [String]
highlights' [] = displays
highlights' [0] = map invertLine . displays
highlights' is = highlights is

highlight' :: Highlight.Highlight a => Highlight a => [Int] -> a -> String
highlight' [] = display
highlight' [0] = invertLine . display
highlight' is = highlight is

class Display a => Highlight a where
  highlights :: [Int] -> a -> [String]
  highlight :: [Int] -> a -> String

data DynHighlight =
  forall a. Highlight a => Hi a

instance Display DynHighlight where
  display (Hi x) = display x
  displays (Hi x) = displays x

instance Highlight DynHighlight where
  highlight is (Hi x) = highlight is x
  highlights is (Hi x) = highlights is x

instance Highlight String where
  highlight is = todo
  highlights is x = todo

instance Highlight a => Highlight (Maybe a) where
  highlight is = todo
  highlights is e = todo

instance Highlight Argument where
  highlight is (Argument name ty) = todo
  highlights is a = todo

instance Highlight [Argument] where
  highlight is = todo
  highlights is _ = todo

instance Highlight [Decl] where
  highlight is = todo
  highlights is = todo

instance Highlight [Statement] where
  highlight is = todo
  highlights is = todo

instance Highlight Bop where
  highlight = todo
  highlights = todo

instance Highlight Uop where
  highlight = todo
  highlights = todo

instance Highlight [Maybe Expr] where
  highlight = todo
  highlights = todo

instance Highlight [Expr] where
  highlight = todo
  highlights = todo

instance Highlight Expr where
  highlight = todo
  highlights = todo

instance Highlight Statement where
  highlight = todo
  highlights = todo

instance Highlight Decl where
  highlight = todo
  highlights = todo
