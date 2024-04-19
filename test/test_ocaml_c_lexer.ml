open Ocaml_c_lexer.Lexer
open Ocaml_c_lexer.Cli
type test_case = { file_name: string; expected: token list; }

let test_cases = [
        {
                file_name = "../examples/hello_world.c";
                expected = [
                        POUND;
                        IDENT "include";
                        LT;
                        IDENT "stdio.h";
                        GT;
                        INTTYPE;
                        IDENT "main";
                        LPAREN;
                        RPAREN;
                        LBRACE;
                        IDENT "printf";
                        LPAREN;
                        STRING "Hello, world!\\n";
                        RPAREN;
                        SEMICOLON;
                        RETURN;
                        INT(0);
                        SEMICOLON;
                        RBRACE;
                        EOF;
                ];
        };
];;

let rec compare expected real =
if expected != real then
        match [expected, real] with
        | [[] , []] -> true
        | [e::es , r::rs] -> (match (e = r) with
                | true -> compare es rs
                | false -> (
                        Printf.printf "Expected %s, but got %s" (tok_to_string e) (tok_to_string r);
                        false
                        ))
        | [e::_, []] -> (Printf.printf "Expected %s and got nothing" (tok_to_string e); false)
        | _ -> (print_endline "Nah"; false) else true;;

let rec run_tests cases = match cases with
        | [] -> (
                print_endline "Success";
                true
        )
                | t :: ts -> 
                match (compare t.expected (t.file_name |> read_file |> tokenizeString)) with
                                | true -> run_tests ts
                | x -> (print_endline "FAILED";
                        pp_list t.expected;
                        print_endline "";
                        pp_list (t.file_name |> read_file |> tokenizeString);
                        x);;

run_tests test_cases;

