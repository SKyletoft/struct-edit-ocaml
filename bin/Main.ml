open Ast

let () = fib |> ts_func |> String.concat "\n" |> print_endline
