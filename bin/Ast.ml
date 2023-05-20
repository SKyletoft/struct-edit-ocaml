type argument = { arg : string option; ty : string option }

and bop =
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
  | And
  | Or
  | Xor
  | BOr
  | BAnd

and uop = Not | BNot

and expr =
  | BinOps of expr option * (bop option * expr option) list
  | Unop of uop option * expr option
  | Ident of string
  | Number of string
  | Bool of bool
  | Call of { func : expr option; args : expr option list }
  | Object of (string option * expr option) list
  | Function of {
      arguments : argument option list;
      return_type : string option;
      body : block;
    }
  | Lambda of {
      arguments : argument option list;
      body : block;
    }

and statement =
  | Expr of expr option
  | Return of expr option
  | If of expr * block * block option
  | Let of string option * string option * expr option
  | Const of string option * string option * expr option

and block = { block : statement list }

and func = {
  name : string option;
  arguments : argument option list;
  return_type : string option;
  body : block;
}

let indent = Printf.sprintf "\t%s"
let indent_ = List.map indent

let or_blank f x =
  match x with
  | Some r -> f r
  | None -> "_"

let rec ts_string_o x =
  match x with
  | Some x -> x
  | None -> "_"

and ts_argument_o x = or_blank ts_argument x

and ts_argument { arg; ty } =
  let a =
    match arg with
    | Some a -> a
    | None -> "_"
  in
  let t =
    match ty with
    | Some t -> Printf.sprintf ": %s" t
    | None -> ": _"
  in
  Printf.sprintf "%s%s" a t

and ts_bop_o x = or_blank ts_bop x

and ts_bop = function
  | LT -> "<"
  | LTE -> "<="
  | GT -> ">"
  | GTE -> ">="
  | Eqish -> "=="
  | Eq -> "==="
  | Neqish -> "!="
  | NEq -> "!=="
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&&"
  | Or -> "||"
  | Xor -> "^"
  | BOr -> "|"
  | BAnd -> "&"

and ts_uop_o x = or_blank ts_uop x

and ts_uop = function
  | Not -> "!"
  | BNot -> "~"

and ts_expr_o x = or_blank ts_expr x

and ts_expr = function
  | BinOps (e, []) -> (or_blank ts_expr) e
  | BinOps (h, (o, e) :: es) ->
      let r = ts_expr (BinOps (e, es)) in
      let l = ts_expr_o h in
      let o = ts_bop_o o in
      Printf.sprintf "%s %s %s" l o r
  | Unop (o, e) ->
      let e = ts_expr_o e in
      let o = ts_uop_o o in
      Printf.sprintf "%s%s" e o
  | Ident s -> s
  | Number n -> n
  | Bool true -> "true"
  | Bool false -> "false"
  | Call { func; args } ->
      let name = ts_expr_o func in
      args
      |> List.map ts_expr_o
      |> String.concat ", "
      |> Printf.sprintf "%s(%s)" name
  | Object _ -> ""
  | Function
      { arguments; return_type; body = { block = body } } ->
      let args_s =
        arguments
        |> List.map ts_argument_o
        |> String.concat ", "
      in
      let body_s =
        body
        |> List.map ts_statement
        |> List.flatten
        |> List.map indent
      in
      let ret =
        match return_type with
        | Some x -> x
        | None -> "_"
      in
      let first =
        Printf.sprintf "function (%s): %s {" args_s ret
      in
      String.concat ""
        (List.append (first :: body_s) [ "}" ])
  | Lambda { arguments; body = { block = body } } ->
      let args_s =
        arguments
        |> List.map ts_argument_o
        |> String.concat ", "
      in
      let body_s =
        body
        |> List.map ts_statement
        |> List.flatten
        |> List.map indent
      in
      let first = Printf.sprintf "(%s) => {" args_s in
      String.concat ""
        (List.append (first :: body_s) [ "}" ])

and ts_statement_o x =
  match x with
  | Some s -> ts_statement s
  | None -> [ "_" ]

and ts_statement = function
  | Expr e -> [ Printf.sprintf "%s;" (ts_expr_o e) ]
  | Return e ->
      [ Printf.sprintf "return %s;" (ts_expr_o e) ]
  | If (cond, { block = ss }, None) ->
      let cond_s = ts_expr cond in
      let then_s =
        ss
        |> List.map ts_statement
        |> List.flatten
        |> List.map indent
      in
      let c = Printf.sprintf "if (%s) {" cond_s in
      let e = "}" in
      c :: List.append then_s [ e ]
  | If (cond, { block = t }, Some { block = e }) ->
      let first =
        If (cond, { block = t }, None)
        |> ts_statement
        (* Drop the last element *)
        |> List.rev
        |> List.tl
        |> List.rev
      in
      let else_ =
        e
        |> List.map ts_statement
        |> List.flatten
        |> List.map indent
      in
      List.append
        (List.append first ("} else {" :: else_))
        [ "}" ]
  | Let (name, ty, Some e) ->
      let n = ts_string_o name in
      let e = ts_expr e in
      let t = ts_string_o ty in
      [ Printf.sprintf "let %s: %s = %s;" n t e ]
  | Let (name, ty, None) ->
      let n = ts_string_o name in
      let t = ts_string_o ty in
      [ Printf.sprintf "let %s: %s;" n t ]
  | Const (name, ty, e) ->
      let n = ts_string_o name in
      let e = ts_expr_o e in
      let t = ts_string_o ty in
      [ Printf.sprintf "let %s: %s = %s;" n t e ]

and ts_func_o x =
  match x with
  | Some x -> x
  | None -> [ "_" ]

and ts_func
    {
      name;
      arguments;
      return_type;
      body = { block = body };
    } =
  let n = ts_string_o name in
  let args_s =
    arguments
    |> List.map ts_argument_o
    |> String.concat ", "
  in
  let r = ts_string_o return_type in
  let body_s =
    body
    |> List.map ts_statement
    |> List.flatten
    |> List.map indent
  in
  let first =
    Printf.sprintf "function %s(%s): %s {" n args_s r
  in
  List.append (first :: body_s) [ "}" ]

(*
  function fib(n: number): number {
    if (n <= 1) {
      return 1;
    } else {
      const prev = fib(n - 1);
      const before_that = fib(n - 2);
      return prev + before_that;
    }
  }
*)
let fib : func =
  {
    name = Some "fib";
    arguments =
      [ Some { arg = Some "n"; ty = Some "number" } ];
    return_type = Some "number";
    body =
      {
        block =
          [
            If
              ( BinOps
                  ( Some (Ident "n"),
                    [ (Some LTE, Some (Number "1")) ] ),
                { block = [ Return (Some (Number "1")) ] },
                Some
                  {
                    block =
                      [
                        Const
                          ( Some "prev",
                            None,
                            Some
                              (Call
                                 {
                                   func = Some (Ident "fib");
                                   args =
                                     [
                                       Some
                                         (BinOps
                                            ( Some
                                                (Ident "n"),
                                              [
                                                ( Some Sub,
                                                  Some
                                                    (Number
                                                       "2")
                                                );
                                              ] ));
                                     ];
                                 }) );
                        Const
                          ( Some "before_that",
                            None,
                            Some
                              (Call
                                 {
                                   func = Some (Ident "fib");
                                   args =
                                     [
                                       Some
                                         (BinOps
                                            ( Some
                                                (Ident "n"),
                                              [
                                                ( Some Sub,
                                                  Some
                                                    (Number
                                                       "1")
                                                );
                                              ] ));
                                     ];
                                 }) );
                        Return
                          (Some
                             (BinOps
                                ( Some (Ident "prev"),
                                  [
                                    ( Some Add,
                                      Some
                                        (Ident "before_that")
                                    );
                                  ] )));
                      ];
                  } );
          ];
      };
  }
