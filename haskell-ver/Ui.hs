module Ui where

import           Debug.Trace
import           GHC.Stack
import           Prelude     hiding (Left, Right)

import           Ast
import           Display     (Display)
import qualified Display
import           Edit        (Action (..), Edit)
import qualified Edit
import           Highlight   (Cursor, Highlight)
import qualified Highlight
import           Navigate    (Direction (..), Navigate)
import qualified Navigate

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
  maxCursor (Ast a) = Navigate.maxCursor a
  validate (Ast a) = Navigate.validate a

render :: HasCallStack => [DynAst] -> Cursor -> IO ()
render as cur = do
  let lines = Highlight.highlights' cur as
  let actions' = Edit.actions . Edit.getChild as $ cur
  mapM_ putStrLn lines

navigate' :: [DynAst] -> Highlight.Cursor -> IO ()
navigate' ast cur = do
  c <- getChar
  let cur' =
        case c of
          'a' -> Navigate.navigate ast Left cur
          'd' -> Navigate.navigate ast Right cur
          'w' -> Navigate.navigate ast Out cur
          's' -> Navigate.navigate ast In cur
          _   -> cur
  let cur'' = reverse . (0:) . reverse $ cur'
  print (Navigate.validate ast cur'', cur')
  render ast cur''
  navigate' ast cur'
