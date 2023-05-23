module Ast where

data Decl
  = Let (Maybe String) (Maybe String) (Maybe Expr)
  | Const (Maybe String) (Maybe String) (Maybe Expr)
  | Func (Maybe String) [Maybe Argument] (Maybe String) [Statement]

data Argument = Argument (Maybe String) (Maybe String)

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

data Uop = Not | BNot

data Expr
  = BinOps (Maybe Expr) [(Maybe Bop, Maybe Expr)]
  | Unop (Maybe Uop) (Maybe Expr)
  | Ident String
  | Number String
  | Boolean Bool
  | Call (Maybe Expr) [Maybe Expr]
  | Object [(Maybe String, Maybe Expr)]
  | Function [Maybe Argument] (Maybe String) [Statement]
  | Lambda [Maybe Argument] [Statement]

data Statement
  = SExpr (Maybe Expr)
  | Return (Maybe Expr)
  | If (Maybe Expr) [Statement] (Maybe [Statement])
  | SDecl Decl
