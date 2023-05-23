open Ast
open Editing

let () =
  fib |> ts_decl |> String.concat "\n" |> print_endline;

  let t = ref [] in
  t := insert_function !t;

  !t
  |> List.map ts_decl
  |> List.flatten
  |> String.concat "\n"
  |> print_endline
