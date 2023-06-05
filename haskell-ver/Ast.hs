module Ast where

data Decl
  = Let
      { dName :: Maybe String
      , dType :: Maybe String
      , dExpr :: Maybe Expr
      }
  | Const
      { dName :: Maybe String
      , dType :: Maybe String
      , dExpr :: Maybe Expr
      }
  | Func
      { dName :: Maybe String
      , dArgs :: [Argument]
      , dType :: Maybe String
      , dBody :: [Statement]
      }
  deriving (Show)

data Argument =
  Argument
    { aName :: Maybe String
    , aExpr :: Maybe String
    }
  deriving (Show)

data Bop
  = LT
  | LTE
  | GT
  | GTE
  | Eqish
  | Eq
  | Neqish
  | NEq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | BOr
  | BAnd
  deriving (Show)

data Uop
  = Neg
  | Not
  | BNot
  deriving (Show)

data Expr
  = BinOps (Maybe Expr) [(Maybe Bop, Maybe Expr)]
  | UnOp (Maybe Uop) (Maybe Expr)
  | Ident
      { cIdent :: String
      }
  | Number
      { cNumber :: String
      }
  | Boolean
      { cBool :: Bool
      }
  | Call
      { cName   :: Maybe Expr
      , cParams :: [Maybe Expr]
      }
  | Object [(Maybe String, Maybe Expr)]
  | Function
      { cArgs  :: [Argument]
      , cRet   :: Maybe String
      , cBlock :: [Statement]
      }
  | Lambda
      { cArgs  :: [Argument]
      , cBlock :: [Statement]
      }
  deriving (Show)

data Statement
  = SExpr (Maybe Expr)
  | Return (Maybe Expr)
  | If
      { iCond :: Maybe Expr
      , iThen :: [Statement]
      , iElse :: Maybe [Statement]
      }
  | SDecl Decl
  deriving (Show)

data Any
  = AnyS Statement
  | AnyE Expr
  | AnyU Uop
  | AnyB Bop
  | AnyA Argument
  | AnyD Decl
