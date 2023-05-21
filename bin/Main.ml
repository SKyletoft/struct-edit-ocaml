open Ast
open Editing

let () =
  fib |> ts_decl |> String.concat "\n" |> print_endline;
