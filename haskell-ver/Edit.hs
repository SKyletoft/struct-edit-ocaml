module Edit where

import           Debug.Trace
import           GHC.Stack

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
  | InsertParameter
  | InsertBinOp Bop
  | InsertUnOp Uop
  | MakeCall
  | MakeBinOp
  | Get
  | Remove
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

editInner' ::
     HasCallStack
  => Edit a =>
       a -> [Int] -> Action -> a
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
  edit _ 0 Remove = Nothing
  edit _ _ a                  = error $ "Invalid action " ++ show a
  editInner _ _ _ = error "Invalid action"

instance Edit [Argument] where
  actions _ = todo
  edit x 0 Get             = x
  edit as i InsertArgument = as ++ [Argument Nothing Nothing]
  edit _ _ a               = error $ "Invalid action " ++ show a
  editInner as (i:is) a =
    let a'
          | length is == 1 = Argument Nothing Nothing
          | otherwise = editInner' (as !! i) is a
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost

instance Edit [Statement] where
  actions _ = todo
  edit s 0 Get = s
  edit ss i a =
    let aPre = take i ss
        aPost = drop (i + 1) ss
        s' =
          case a of
            InsertIf -> If Nothing [] Nothing
            InsertReturn -> Return Nothing
            InsertConst -> SDecl (Const Nothing Nothing Nothing)
            InsertLet -> SDecl (Let Nothing Nothing Nothing)
            _ -> error $ "Invalid action " ++ show a ++ ", " ++ show ss
     in aPre ++ [s'] ++ aPost
  editInner as (i:is) a =
    let a' = editInner' (as !! i) is a
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost

instance Edit [Decl] where
  actions _ = todo
  edit _ _ _ = todo
  editInner ds (i:is) a =
    let d' = editInner' (ds !! i) is a
        dPre = take i ds
        dPost = drop (i + 1) ds
     in dPre ++ [d'] ++ dPost

instance Edit Decl where
  actions _ = todo
  edit x 0 Get = x
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner c@(Const n _ _) (0:is) a = c {dName = editInner' n is a}
  editInner c@(Const _ t _) (1:is) a = c {dType = editInner' t is a}
  editInner c@(Const _ _ e) (2:is) a = c {dExpr = editInner' e is a}
  editInner l@(Let n _ _) (0:is) a = l {dName = editInner' n is a}
  editInner l@(Let _ t _) (1:is) a = l {dType = editInner' t is a}
  editInner l@(Let _ _ e) (2:is) a = l {dExpr = editInner' e is a}
  editInner f@(Func n _ _ _) (0:is) a = f {dName = editInner' n is a}
  editInner f@(Func _ as _ _) (1:is) a = f {dArgs = editInner' as is a}
  editInner f@(Func _ _ r _) (2:is) a = f {dType = editInner' r is a}
  editInner f@(Func _ _ _ b) (3:is) a = f {dBody = editInner' b is a}
  editInner f is a = error $ display f ++ ", " ++ show is ++ ", " ++ show a

instance Edit Argument where
  actions _ = todo
  edit x 0 Get = x
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner as@(Argument n _) (0:is) a = as {aName = editInner' n is a}
  editInner as@(Argument _ e) (1:is) a = as {aExpr = editInner' e is a}
  editInner _ _ _                      = todo

instance Edit (Maybe Bop) where
  actions _ = todo
  edit _ 0 (InsertBinOp o) = Just o
  edit _ 0 Remove = Nothing
  edit o i a               = error $ show a ++ ", " ++ show i ++ ", " ++ show a
  editInner _ _ _ = todo

instance Edit (Maybe Expr) where
  actions _ = todo
  edit x 0 Get = x
  edit _ 0 InsertTrue = Just (Boolean True)
  edit _ 0 InsertFalse = Just (Boolean False)
  edit _ 0 (InsertIdent i) = Just (Ident i)
  edit _ 0 (InsertNumber n) = Just (Number n)
  edit _ 0 Remove = Nothing
  edit e _ a = error $ "Invalid action " ++ show a ++ ", " ++ show e
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"

instance Edit (Maybe [Expr]) where
  actions _ = todo
  edit x 0 Get = x
  edit _ 0 Remove = Nothing
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"

instance Edit (Maybe [Statement]) where
  actions _ = todo
  edit x 0 Get              = x
  edit Nothing 0 InsertElse = Just []
  edit _ 0 Remove = Nothing
  edit _ _ a                = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"

instance Edit [Expr] where
  actions _ = todo
  edit x i a = error $ show x ++ ", " ++ show i ++ ", " ++ show a
  editInner es (i:is) a =
    let e' = editInner' (es !! i) is a
        preEs = take i es
        postEs = drop (i + 1) es
     in preEs ++ [e'] ++ postEs

instance Edit [Maybe Expr] where
  actions _ = todo
  edit x i a = error $ show x ++ ", " ++ show i ++ ", " ++ show a
  editInner es (i:is) a =
    let e' = editInner' (es !! i) is a
        preEs = take i es
        postEs = drop (i + 1) es
     in preEs ++ [e'] ++ postEs

instance Edit Expr where
  actions _ = todo
  edit x 0 Get       = x
  edit x 0 MakeBinOp = BinOps (Just x) [(Nothing, Nothing)]
  edit x 0 MakeCall  = Call (Just x) []
  edit e i a         = error $ show (e, i, a)
  editInner e@(BinOps l rs) (0:is) a = BinOps (editInner' l is a) rs
  editInner e@(BinOps l rs) (i:is) a =
    let i' = (i - 1) `div` 2
        preRs = take i' rs
        postRs = drop (i' + 1) rs
        (o, r) = rs !! i'
        (o', r')
          | odd i =
            let o' = editInner' o is a
             in (o', r)
          | even i =
            let r' = editInner' r is a
             in (o, r')
     in BinOps l (preRs ++ [(o', r')] ++ postRs)
  editInner e@(Call n _) (0:is) a = e {cName = editInner' n is a}
  editInner e@(Call _ as) (1:is) a = e {cParams = editInner' as is a}
  editInner e is a = error $ show e ++ ",\n" ++ show is ++ ",\n" ++ show a

instance Edit Statement where
  actions _ = todo
  edit x 0 Get = x
  edit i@(If _ _ Nothing) 0 InsertElse = i {iElse = Just []}
  edit s _ a = error $ "Invalid action " ++ show a ++ ", " ++ show s
  editInner (SExpr e) (0:is) a    = SExpr $ editInner' e is a
  editInner (Return e) (0:is) a   = Return $ editInner' e is a
  editInner i@(If c _ _) (0:is) a = i {iCond = editInner' c is a}
  editInner i@(If _ t _) (1:is) a = i {iThen = editInner' t is a}
  editInner i@(If _ _ e) (2:is) a = i {iElse = editInner' e is a}
  editInner (SDecl d) (0:is) a    = SDecl $ editInner' d is a
  editInner s i a = error $ show (s, i, a)
