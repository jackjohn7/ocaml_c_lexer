open Ocaml_c_lexer.Lexer
open Ocaml_c_lexer.Cli
let main file_name = file_name |> read_file |> tokenizeString |> pp_list;;

let () = match Array.length Sys.argv with
        | 2 -> main Sys.argv.(1)
        | _ -> failwith (Printf.sprintf "Usage: %s <file_name>\n" Sys.argv.(0))
