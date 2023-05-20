open Ast
open Editing

let () = fib |> ts_func |> String.concat "\n" |> print_endline
