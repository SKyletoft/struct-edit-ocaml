open Ast

let insert_function tree =
  List.append tree
    [
      Func
        {
          name = None;
          arguments = [];
          return_type = None;
          body = { block = [] };
        };
    ]

let insert_decl_name d n =
  match d with
  | Func { name = _; arguments; return_type; body } ->
      Func { name = Some n; arguments; return_type; body }
  | Let (_, ty, value) -> Let (Some n, ty, value)
  | Const (_, ty, value) -> Const (Some n, ty, value)

let insert_decl_type d t =
  match d with
  | Func { name; arguments; return_type = _; body } ->
      Func { name; arguments; return_type = Some t; body }
  | Let (name, _, value) -> Let (name, Some t, value)
  | Const (name, _, value) -> Const (name, Some t, value)
