module Edit where

import           Debug.Trace

import           Ast
import           Display     hiding (todo)

todo = error "Not yet implemented"

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y : xs
insertAt n y (x:xs) = y : insertAt (n - 1) y xs

data Action
  = EditText String
  | InsertIf
  | InsertReturn
  | InsertLet
  | InsertConst
  | InsertFunc
  | InsertArgument
  | InsertNumber String
  | InsertTrue
  | InsertFalse
  | InsertElse
  | InsertIdent String
  | InsertBinOp Bop
  | InsertUnOp Uop
  | MakeCall
  | MakeBinOp
  | Get
  deriving (Show)

class Display a =>
      Edit a
  where
  edit :: a -> Int -> Action -> a
  editInner :: a -> [Int] -> Action -> a
  actions :: a -> [Action]

data DynEdit =
  forall a. (Edit a, Display a) =>
            Ed a

instance Display DynEdit where
  display (Ed e) = display e
  displays (Ed e) = displays e

instance Edit DynEdit where
  edit (Ed e) i a = Ed (edit e i a)
  editInner (Ed e) i a = Ed (editInner e i a)
  actions (Ed e) = actions e

editInner' :: Edit a => a -> [Int] -> Action -> a
editInner' _ [] _ = error "No more indicies"
editInner' x [i] a =
  id
  -- . trace ("\nEditing actual: " ++ display x ++ "(" ++ show i ++ "), " ++ show a ++ "\n")
   $
  edit x i a
editInner' x is@(i:_) a =
  id
  -- . trace ("\nEditing inner:  " ++ display x ++ "(" ++ show i ++ "), " ++ show a ++ "\n")
   $
  editInner x is a

visit :: Edit a => a -> Int -> a
visit x i = edit x i Get

instance Edit (Maybe String) where
  actions _ = todo
  edit x 0 Get                = x
  edit _ 0 (EditText newName) = Just newName
  edit _ _ a                  = error $ "Invalid action " ++ show a
  editInner _ _ _ = error "Invalid action"

instance Edit [Argument] where
  actions _ = todo
  edit :: [Argument] -> Int -> Action -> [Argument]
  edit x 0 Get             = x
  edit as i InsertArgument = as ++ [Argument Nothing Nothing]
  edit _ _ a               = error $ "Invalid action " ++ show a
  editInner as [i, y] a =
    let a' = Argument Nothing Nothing
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost
  editInner as (i:is) a =
    let a' = editInner' (as !! i) is a
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost

instance Edit [Statement] where
  actions _ = todo
  edit s 0 Get = s
  edit ss i InsertIf =
    let s' = If Nothing [] Nothing
        aPre = take i ss
        aPost = drop (i + 1) ss
     in aPre ++ [s'] ++ aPost
  edit _ _ a = error $ "Invalid action " ++ show a
  editInner as (i:is) a =
    let a' = editInner' (as !! i) is a
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost

instance Edit Decl where
  actions _ = todo
  edit x 0 Get = x
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner f@(Func n _ _ _) (0:is) a =
    let n' = editInner' n is a
     in f {dName = n'}
  editInner f@(Func _ as _ _) (1:is) a =
    let as' = editInner' as is a
     in f {dArgs = as'}
  editInner f@(Func _ _ r _) (2:is) a =
    let r' = editInner' r is a
     in f {dType = r'}
  editInner f@(Func _ _ _ b) (3:is) a =
    let b' = editInner' b is a
     in f {dBody = b'}
  editInner f is a = error $ display f ++ ", " ++ show is ++ ", " ++ show a

instance Edit Argument where
  actions _ = todo
  edit x 0 Get = x
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner as@(Argument n _) (0:is) a =
    let n' = editInner' n is a
     in as {aName = n'}
  editInner as@(Argument _ e) (1:is) a =
    let e' = editInner' e is a
     in as {aExpr = e'}
  editInner _ _ _ = todo

instance Edit (Maybe Expr) where
  actions _ = todo
  edit x 0 Get                   = x
  edit Nothing 0 InsertTrue      = Just (Boolean True)
  edit Nothing 0 InsertFalse     = Just (Boolean False)
  edit Nothing 0 (InsertIdent i) = Just (Ident i)
  edit _ _ a                     = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"

instance Edit (Maybe [Statement]) where
  actions _ = todo
  edit x 0 Get              = x
  edit Nothing 0 InsertElse = Just []
  edit _ _ a                = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"

instance Edit Expr where
  actions _ = todo
  edit x 0 Get       = x
  edit x 0 MakeBinOp = BinOps (Just x) [(Nothing, Nothing)]
  edit _ _ a         = error $ "Invalid action " ++ show a
  editInner _ _ _ = error "hi"

instance Edit Statement where
  actions _ = todo
  edit x 0 Get = x
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner (SExpr e) (0:is) a = SExpr $ editInner' e is a
  editInner (Return e) (0:is) a = Return $ editInner' e is a
  editInner i@(If c _ _) (0:is) a =
    let c' = editInner' c is a
     in i {iCond = c'}
  editInner i@(If _ t _) (1:is) a =
    let t' = editInner' t is a
     in i {iThen = t'}
  editInner i@(If _ _ e) (2:is) a =
    let e' = editInner' e is a
     in i {iElse = e'}
  editInner (SDecl d) (0:is) a = SDecl $ editInner' d is a
