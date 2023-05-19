type ty = { name : string }
type ident = { name : string }
type argument = { name : ident; typ : ty }

type token =
  | Ident of ident
  | Number of string
  | Bool of bool
  | LT
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

type statement =
  | BinOps of token list
  | Unops of token * statement
  | Return of statement
  | Token of token

type block = { block : statement list }

type func = {
  name : ident;
  arguments : argument list;
  return_type : ty;
  body : block;
}

type if_ = { condition : statement; then_ : block; else_ : block option }
