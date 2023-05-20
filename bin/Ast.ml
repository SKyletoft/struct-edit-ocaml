type argument = { arg : string; ty : string }

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
  | BinOps of expr * (bop * expr) list
  | Unop of uop * expr
  | Ident of string
  | Number of string
  | Bool of bool
  | Call of { func : expr; args : expr list }
  | Object of (string * expr) list
  | Function of {
      arguments : argument list;
      return_type : string;
      body : block;
    }
  | Lambda of { arguments : argument list; return_type : string; body : block }

and statement =
  | Expr of expr
  | Return of expr
  | If of expr * block * block option
  | Let of string * string option * expr option
  | Const of string * string option * expr

and block = { block : statement list }

and func = {
  name : string;
  arguments : argument list;
  return_type : string;
  body : block;
}

let indent = Printf.sprintf "\t%s"
let indent_ = List.map indent

let rec ts_argument { arg; ty } = Printf.sprintf "%s: %s" arg ty

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

and ts_uop = function Not -> "!" | BNot -> "~"

and ts_expr = function
  | BinOps (e, []) -> ts_expr e
  | BinOps (h, (o, e) :: es) ->
      let r = ts_expr (BinOps (e, es)) in
      let l = ts_expr h in
      let o = ts_bop o in
      Printf.sprintf "%s %s %s" l o r
  | Unop (o, e) ->
      let e = ts_expr e in
      let o = ts_uop o in
      Printf.sprintf "%s%s" e o
  | Ident s -> s
  | Number n -> n
  | Bool true -> "true"
  | Bool false -> "false"
  | Call { func; args } ->
      let name = ts_expr func in
      args |> List.map ts_expr |> String.concat ", "
      |> Printf.sprintf "%s(%s)" name
  | Object _ -> ""
  | Function { arguments = _; return_type = _; body = _ } -> ""
  | Lambda { arguments = _; return_type = _; body = _ } -> ""

and ts_statement = function
  | Expr e -> [ Printf.sprintf "%s;" (ts_expr e) ]
  | Return e -> [ Printf.sprintf "return %s;" (ts_expr e) ]
  | If (cond, { block = ss }, None) ->
      let cond_s = ts_expr cond in
      let then_s =
        ss |> List.map ts_statement |> List.flatten |> List.map indent
      in
      let c = Printf.sprintf "if (%s) {" cond_s in
      let e = "}" in
      c :: List.append then_s [ e ]
  | If (cond, { block = t }, Some { block = e }) ->
      let first =
        If (cond, { block = t }, None)
        |> ts_statement |> List.rev (* Drop the last element *) |> List.tl
        |> List.rev
      in
      let else_ =
        e |> List.map ts_statement |> List.flatten |> List.map indent
      in
      List.append (List.append first ("} else {" :: else_)) [ "}" ]
  | Let (name, ty, Some e) ->
      let e = ts_expr e in
      let t =
        match ty with Some t -> Printf.sprintf ": %s" t | None -> ""
      in
      [ Printf.sprintf "let %s%s = %s;" name t e ]
  | Let (name, ty, None) ->
      let t =
        match ty with Some t -> Printf.sprintf ": %s" t | None -> ""
      in
      [ Printf.sprintf "let %s%s;" name t ]
  | Const (name, ty, e) ->
      let e = ts_expr e in
      let t =
        match ty with Some t -> Printf.sprintf ": %s" t | None -> ""
      in
      [ Printf.sprintf "let %s%s = %s;" name t e ]

and ts_func { name; arguments; return_type; body = { block = body } } =
  let args_s = arguments |> List.map ts_argument |> String.concat ", " in
  let body_s =
    body |> List.map ts_statement |> List.flatten |> List.map indent
  in
  let first = Printf.sprintf "function %s(%s): %s {" name args_s return_type in
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
    name = "fib";
    arguments = [ { arg = "n"; ty = "number" } ];
    return_type = "number";
    body =
      {
        block =
          [
            If
              ( BinOps (Ident "n", [ (LTE, Number "1") ]),
                { block = [ Return (Number "1") ] },
                Some
                  {
                    block =
                      [
                        Const
                          ( "prev",
                            None,
                            Call
                              {
                                func = Ident "fib";
                                args =
                                  [ BinOps (Ident "n", [ (Sub, Number "2") ]) ];
                              } );
                        Const
                          ( "before_that",
                            None,
                            Call
                              {
                                func = Ident "fib";
                                args =
                                  [ BinOps (Ident "n", [ (Sub, Number "1") ]) ];
                              } );
                        Return
                          (BinOps (Ident "prev", [ (Add, Ident "before_that") ]));
                      ];
                  } );
          ];
      };
  }
