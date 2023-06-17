module Edit where

import           Debug.Trace
import           GHC.Stack
import           Prelude     hiding ((!!))

import           Ast
import           Display     hiding (todo)

infixl 9 !!

(!!) :: HasCallStack => [a] -> Int -> a
[] !! _     = error "(!!): index out of scope!"
(x:_) !! 0  = x
(_:xs) !! n = xs !! (n - 1)

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
  | Remove
  deriving (Show)

class Display a =>
      Edit a
  where
  edit      :: HasCallStack => a -> Int -> Action -> a
  editInner :: HasCallStack => a -> [Int] -> Action -> a
  actions   :: HasCallStack => a -> [Action]
  getChild  :: HasCallStack => a -> [Int] -> DynEdit

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
  getChild (Ed e) = getChild e

editInner' ::
     HasCallStack
  => Show a =>
       Edit a =>
         a -> [Int] -> Action -> a
editInner' _ [] _ = error "No more indicies"
editInner' x [i] a =
  id
  -- . trace ("\nEditing actual: " ++ show x ++ "(" ++ show i ++ "), " ++ show a ++ "")
   $
  edit x i a
editInner' x is@(i:_) a =
  id
  -- . trace ("\nEditing inner:  " ++ show x ++ "(" ++ show i ++ "), " ++ show a ++ "")
   $
  editInner x is a

instance Edit String where
  actions _ = todo
  edit _ _ _ = todo
  editInner _ _ _ = todo
  getChild _ _ = todo

instance Edit (Maybe String) where
  actions _ = todo
  edit _ 0 (EditText newName) = Just newName
  edit _ 0 Remove             = Nothing
  edit _ _ a                  = error $ "Invalid action " ++ show a
  editInner _ _ _ = error "Invalid action"
  getChild x [] = Ed x
  getChild (Just x) (0:is) = Ed x

instance Edit [Argument] where
  actions _ = todo
  edit as i InsertArgument = as ++ [Argument Nothing Nothing]
  edit _ _ a               = error $ "Invalid action " ++ show a
  editInner as (i:is) a =
    let a'
          | length is == 1 = Argument Nothing Nothing
          | otherwise = editInner' (as !! i) is a
        aPre = take i as
        aPost = drop (i + 1) as
     in aPre ++ [a'] ++ aPost
  getChild x [] = Ed x
  getChild x (i:is) = Ed $ x !! i

instance Edit [Statement] where
  actions _ = todo
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
  getChild x [] = Ed x
  getChild x (i:is) = Ed $ x !! i

instance Edit [Decl] where
  actions _ = todo
  edit _ _ _ = todo
  editInner ds (i:is) a =
    let d' = editInner' (ds !! i) is a
        dPre = take i ds
        dPost = drop (i + 1) ds
     in dPre ++ [d'] ++ dPost
  getChild x [] = Ed x
  getChild x (i:is) = Ed $ x !! i

instance Edit Decl where
  actions _ = todo
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner d (i:is) a = case d of
    Const n _ _   | i == 0 -> d {dName = editInner' n is a}
    Const _ t _   | i == 1 -> d {dType = editInner' t is a}
    Const _ _ e   | i == 2 -> d {dExpr = editInner' e is a}
    Let n _ _     | i == 0 -> d {dName = editInner' n is a}
    Let _ t _     | i == 1 -> d {dType = editInner' t is a}
    Let _ _ e     | i == 2 -> d {dExpr = editInner' e is a}
    Func n _ _ _  | i == 0 -> d {dName = editInner' n is a}
    Func _ as _ _ | i == 1 -> d {dArgs = editInner' as is a}
    Func _ _ r _  | i == 2 -> d {dType = editInner' r is a}
    Func _ _ _ b  | i == 3 -> d {dBody = editInner' b is a}
  editInner f is a = error $ display f ++ ", " ++ show is ++ ", " ++ show a
  getChild x [] = Ed x
  getChild x (i:is) = case x of
    Const n _ _   | i == 0 -> Ed n
    Const _ t _   | i == 1 -> Ed t
    Const _ _ e   | i == 2 -> Ed e
    Let n _ _     | i == 0 -> Ed n
    Let _ t _     | i == 1 -> Ed t
    Let _ _ e     | i == 2 -> Ed e
    Func n _ _ _  | i == 0 -> Ed n
    Func _ as _ _ | i == 1 -> Ed as
    Func _ _ r _  | i == 2 -> Ed r
    Func _ _ _ b  | i == 3 -> Ed b

instance Edit Argument where
  actions _ = todo
  edit _ _ a   = error $ "Invalid action " ++ show a
  editInner as@(Argument n _) (0:is) a = as {aName = editInner' n is a}
  editInner as@(Argument _ e) (1:is) a = as {aExpr = editInner' e is a}
  editInner _ _ _                      = todo
  getChild x [] = Ed x
  getChild (Argument n e) (i:is)
    | i == 0 = Ed n
    | i == 1 = Ed e

instance Edit (Maybe Uop) where
  actions _ = todo
  edit _ 0 (InsertUnOp o) = Just o
  edit _ 0 Remove          = Nothing
  edit o i a               = error $ show a ++ ", " ++ show i ++ ", " ++ show a
  editInner _ _ _ = todo
  getChild x [] = Ed x

instance Edit (Maybe Bop) where
  actions _ = todo
  edit _ 0 (InsertBinOp o) = Just o
  edit _ 0 Remove          = Nothing
  edit o i a               = error $ show a ++ ", " ++ show i ++ ", " ++ show a
  editInner _ _ _ = todo
  getChild x [] = Ed x

instance Edit (Maybe Expr) where
  actions _ = todo
  edit _ 0 InsertTrue = Just (Boolean True)
  edit _ 0 InsertFalse = Just (Boolean False)
  edit _ 0 (InsertIdent i) = Just (Ident i)
  edit _ 0 (InsertNumber n) = Just (Number n)
  edit _ 0 Remove = Nothing
  edit e _ a = error $ "Invalid action " ++ show a ++ ", " ++ show e
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"
  getChild x [] = Ed x
  getChild (Just x) (0:is) = Ed x

instance Edit (Maybe [Expr]) where
  actions _ = todo
  edit _ 0 Remove = Nothing
  edit _ _ a      = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"
  getChild x [] = Ed x
  getChild (Just x) (0:is) = Ed x

instance Edit (Maybe [Statement]) where
  actions _ = todo
  edit Nothing 0 InsertElse = Just []
  edit _ 0 Remove           = Nothing
  edit _ _ a                = error $ "Invalid action " ++ show a
  editInner (Just e) (0:is) a = Just $ editInner' e is a
  editInner Nothing (0:is) a  = error "Entering nothing"
  getChild x [] = Ed x
  getChild (Just x) (0:is) = Ed x

instance Edit [Expr] where
  actions _ = todo
  edit x i a = error $ show x ++ ", " ++ show i ++ ", " ++ show a
  editInner es (i:is) a =
    let e' = editInner' (es !! i) is a
        preEs = take i es
        postEs = drop (i + 1) es
     in preEs ++ [e'] ++ postEs
  getChild x [] = Ed x
  getChild x (i:is) = Ed $ x !! i

instance Edit [Maybe Expr] where
  actions _ = todo
  edit x i a = error $ show x ++ ", " ++ show i ++ ", " ++ show a
  editInner es (i:is) a =
    let e' = editInner' (es !! i) is a
        preEs = take i es
        postEs = drop (i + 1) es
     in preEs ++ [e'] ++ postEs
  getChild x [] = Ed x
  getChild x (i:is) = Ed $ x !! i

instance Edit Expr where
  actions _ = todo
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
  getChild x [] = Ed x
  getChild x (i:is) = case x of
    BinOps l _ | i == 0 -> Ed l
    BinOps _ ((o, _):_) | i == 1 -> Ed o
    BinOps _ ((_, r):_) | i == 2 -> Ed r
    BinOps _ (_:rs) | i > 2 -> getChild x ((i - 2):is)
    UnOp o _ | i == 0 -> Ed o
    UnOp _ e | i == 1 -> Ed e

instance Edit Statement where
  actions _ = todo
  edit i@(If _ _ Nothing) 0 InsertElse = i {iElse = Just []}
  edit s _ a = error $ "Invalid action " ++ show a ++ ", " ++ show s
  editInner s (i:is) a = case s of
    SExpr e  | i == 0 -> SExpr $ editInner' e is a
    Return e | i == 0 -> Return $ editInner' e is a
    If c _ _ | i == 0 -> s {iCond = editInner' c is a}
    If _ t _ | i == 1 -> s {iThen = editInner' t is a}
    If _ _ e | i == 2 -> s {iElse = editInner' e is a}
    SDecl d  | i == 0 -> SDecl $ editInner' d is a
    _ -> error $ show (s, i, a)
  getChild s [] = Ed s
  getChild s (i:is) = case s of
    SExpr e  | i == 0 -> Ed e
    Return e | i == 0 -> Ed e
    If c _ _ | i == 0 -> Ed c
    If _ t _ | i == 1 -> Ed t
    If _ _ e | i == 2 -> Ed e
    SDecl d  | i == 0 -> Ed d
    _ -> error $ show (s, i)
