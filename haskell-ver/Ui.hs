module Ui where

import           Prelude   hiding (Left, Right)

import           Ast
import qualified Edit
import           Edit      (Edit, Action(..))
import qualified Display
import           Display   (Display)
import qualified Highlight
import           Highlight (Cursor, Highlight)
import qualified Navigate
import           Navigate  (Navigate, Direction(..))

data DynAst =
  forall a. (Highlight a, Edit a, Display a, Navigate a) =>
            Ast a

instance Display DynAst where
  display (Ast e) = Display.display e
  displays (Ast e) = Display.displays e

instance Display [DynAst] where
  display = unlines . map Display.display
  displays = concatMap Display.displays

instance Edit DynAst where
  edit (Ast e) i a = Ast (Edit.edit e i a)
  editInner (Ast e) i a = Ast (Edit.editInner e i a)
  actions (Ast e) = Edit.actions e
  getChild (Ast e) = Edit.getChild e

instance Edit [DynAst] where
  edit = Highlight.todo
  editInner = Highlight.todo
  actions = Highlight.todo
  getChild = Highlight.todo

instance Highlight DynAst where
  highlight is (Ast a) = Highlight.highlight is a
  highlights is (Ast a) = Highlight.highlights is a

instance Navigate DynAst where
  navigate (Ast a) = Navigate.navigate a

render :: [DynAst] -> Cursor -> IO ()
render as cur = do
  let lines = Highlight.highlights' cur as
  let actions' = Edit.actions . Edit.getChild as $ cur
  mapM_ putStrLn lines

navigate :: DynAst -> Highlight.Cursor -> IO ()
navigate ast cur = do
  c <- getChar
  let cur' =
        case c of
          'a' -> Navigate.navigate ast cur Left
          'd' -> Navigate.navigate ast cur Right
          'w' -> Navigate.navigate ast cur In
          's' -> Navigate.navigate ast cur Out
          _   -> cur
  render [ast] cur'
  navigate ast cur'
