open Ocaml_c_lexer.Lexer
let read_file filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let contents = read_lines [] in
  close_in channel;
  String.concat "\n" contents
let main file_name = file_name |> read_file |> tokenizeString |> pp_list;;

let () = match Array.length Sys.argv with
        | 2 -> main Sys.argv.(1)
        | _ -> failwith (Printf.sprintf "Usage: %s <file_name>\n" Sys.argv.(0))
