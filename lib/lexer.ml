type token =
 | EOF
 | IDENT of string
 | ILLEGAL of string
 | INT of int
 | FLOAT of float
 | CHAR of char
 | STRING of string
 (* Syntax *)
 | LPAREN
 | RPAREN
 | LBRACE
 | RBRACE
 | LBRACKET
 | RBRACKET
 | COMMA
 | SEMICOLON | COLON
 (* Operators *)
 | ASSIGN
 | PLUS
 | ASSIGN_PLUS
 | MINUS
 | ASSIGN_MINUS
 | MOD
 | ASSIGN_MOD
 | INC
 | DEC
 | STAR
 | ASSIGN_MUL
 | DIV
 | ASSIGN_DIV
 | EQ
 | NEQ
 | NOT
 | GT
 | LT
 | GTE
 | LTE
 | AND
 | OR
 | POUND (* for macros *)
 | BWAND
 | ASSIGN_BWAND
 | BWOR
 | ASSIGN_BWOR
 | BWXOR
 | ASSIGN_BWXOR
 | BWOCOMP
 | BWLSHIFT
 | ASSIGN_BWLSHIFT
 | BWRSHIFT
 | ASSIGN_BWRSHIFT
 | SIZEOF
 (* Keywords *)
 | RETURN
 | TYPEDEF
 | UNION
 | UNSIGNED
 | CONST
 | FOR
 | WHILE
 | DO
 (* Primitive types *)
 | INTTYPE
 | FLOATTYPE
 | CHARTYPE
 | STRUCTTYPE
 | ENUMTYPE;;

let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let implode chars =
        let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf;;

exception UnwrapErr;;

let unwrap_int op = match op with
        | Some x -> x
        | None -> raise UnwrapErr;;

let unwrap_float op = match op with
        | Some x -> x
        | None -> raise UnwrapErr;;

let tokenizeString str =
(* For speed, build in reverse and reverse list afterward *)
let rec 
    tokenize chars acc = match chars with
        | [] -> EOF::acc
            (* Ignore whitespace *)
        | (' '::cs) -> tokenize cs acc
        | ('\t'::cs) -> tokenize cs acc
        | ('\n'::cs) -> tokenize cs acc
        | ('('::cs) -> tokenize cs (LPAREN::acc)
        | (')'::cs) -> tokenize cs (RPAREN::acc)
        | ('{'::cs) -> tokenize cs (LBRACE::acc)
        | ('}'::cs) -> tokenize cs (RBRACE::acc)
        | ('['::cs) -> tokenize cs (LBRACKET::acc)
        | (']'::cs) -> tokenize cs (RBRACKET::acc)
        | (','::cs) -> tokenize cs (COMMA::acc)
        | (';'::cs) -> tokenize cs (SEMICOLON::acc)
        | (':'::cs) -> tokenize cs (COLON::acc)
        | ('='::('=')::cs) -> tokenize cs (EQ::acc)
        | ('!'::('=')::cs) -> tokenize cs (NEQ::acc)
        | ('!'::cs) -> tokenize cs (NOT::acc)
        | ('='::cs) -> tokenize cs (ASSIGN::acc)
        | ('<'::('<')::('=')::cs) -> tokenize cs (ASSIGN_BWLSHIFT::acc)
        | ('<'::('<')::cs) -> tokenize cs (BWLSHIFT::acc)
        | ('<'::('=')::cs) -> tokenize cs (LTE::acc)
        | ('<'::cs) -> tokenize cs (LT::acc)
        | ('>'::('>')::('=')::cs) -> tokenize cs (ASSIGN_BWRSHIFT::acc)
        | ('>'::('>')::cs) -> tokenize cs (BWRSHIFT::acc)
        | ('>'::('=')::cs) -> tokenize cs (GTE::acc)
        | ('>'::cs) -> tokenize cs (GT::acc)
        | ('^'::('=')::cs) -> tokenize cs (ASSIGN_BWXOR::acc)
        | ('^'::cs) -> tokenize cs (BWXOR::acc)
        | ('+'::('=')::cs) -> tokenize cs (ASSIGN_PLUS::acc)
        | ('+'::('+')::cs) -> tokenize cs (INC::acc)
        | ('+'::cs) -> tokenize cs (PLUS::acc)
        | ('-'::('=')::cs) -> tokenize cs (ASSIGN_MINUS::acc)
        | ('-'::('-')::cs) -> tokenize cs (DEC::acc)
        | ('-'::cs) -> tokenize cs (MINUS::acc) (* no negatives? *)
        (* Could later match on last acc token (Ident) and specify ptr star *)
        | ('*'::('=')::cs) -> tokenize cs (ASSIGN_MUL::acc)
        | ('*'::cs) -> tokenize cs (STAR::acc)
        | ('/'::('=')::cs) -> tokenize cs (ASSIGN_DIV::acc)
        | ('/'::cs) -> tokenize cs (DIV::acc)
        | ('%'::('=')::cs) -> tokenize cs (ASSIGN_MOD::acc)
        | ('%'::cs) -> tokenize cs (MOD::acc)
        | ('~'::cs) -> tokenize cs (BWOCOMP::acc)
        | ('#'::cs) -> tokenize cs (POUND::acc)
        | ('&'::('&')::cs) -> tokenize cs (AND::acc)
        | ('&'::('=')::cs) -> tokenize cs (ASSIGN_BWAND::acc)
        | ('&'::cs) -> tokenize cs (BWAND::acc)
        | ('|'::('|')::cs) -> tokenize cs (OR::acc)
        | ('|'::('=')::cs) -> tokenize cs (ASSIGN_BWOR::acc)
        | ('|'::cs) -> tokenize cs (BWOR::acc)
        | (('s')::('i')::('z')::('e')::('o')::('f')::cs) ->
            tokenize cs (SIZEOF::acc)
        (* Keywords *)
        | (('r')::('e')::('t')::('u')::('r')::('n')::cs) ->
            tokenize cs (RETURN::acc)
        | (('u')::('n')::('i')::('o')::('n')::cs) ->
            tokenize cs (UNION::acc)
        | (('t')::('y')::('p')::('e')::('d')::('e')::('f')::cs) ->
            tokenize cs (TYPEDEF::acc)
        | (('u')::('n')::('s')::('i')::('g')::('n')::('e')::('d')::cs) ->
            tokenize cs (TYPEDEF::acc)
        | (('i')::('n')::('t')::(' '|'\t')::cs) ->
            tokenize cs (INTTYPE::acc)
        | (('f')::('l')::('o')::('a')::('t')::cs) ->
            tokenize cs (FLOATTYPE::acc)
        | (('c')::('h')::('a')::('r')::cs) ->
            tokenize cs (CHARTYPE::acc)
        | (('s')::('t')::('r')::('u')::('c')::('t')::cs) ->
            tokenize cs (STRUCTTYPE::acc)
        | (('e')::('n')::('u')::('m')::cs) ->
            tokenize cs (ENUMTYPE::acc)
        | (('c')::('o')::('n')::('s')::('t')::cs) ->
            tokenize cs (CONST::acc)
        | (('f')::('o')::('r')::cs) ->
            tokenize cs (FOR::acc)
        | (('w')::('h')::('i')::('l')::('e')::cs) ->
            tokenize cs (WHILE::acc)
        | (('d')::('o')::cs) ->
            tokenize cs (DO::acc)
        (* literals and idents *)
        | (('\'')::c::('\'')::cs) -> tokenize cs ((CHAR c)::acc)
        | (('\"')::cs) -> read_string cs acc []
        | (('0')::('x')::cs) -> read_hex cs acc []
        | (('0')::('b')::cs) -> read_bin cs acc []
            | (('0')::cs) -> read_oct cs acc ['0'; 'o' ; '0']
        | c::cs ->
          if ((Char.code c) >= 65 && (Char.code c) <= 90) ||
          ((Char.code c) >= 97 && (Char.code c) <= 122) then
            read_ident cs acc [c]
          else if ((Char.code c) >= 48 && (Char.code c) <= 57) then
            read_number  cs acc [c]
          else tokenize cs acc
  and
    (* Read float chars to float *)
    read_float cs acc str_acc = match cs with
        | c :: cs when Char.code c >= 48 && Char.code c <= 57 -> read_float cs acc (c::str_acc)
        | cs -> tokenize cs ((FLOAT (unwrap_float
    (float_of_string_opt (implode (List.rev str_acc)))))::acc)
  and
    (* Read numbers and read float if "." is met *)
    read_number cs acc str_acc = match cs with
        | c :: cs when Char.code c >= 48 && Char.code c <= 57 -> read_number cs acc (c::str_acc)
        | '.' as c :: cs -> read_float cs acc (c::str_acc)
        | cs -> tokenize cs ((INT (unwrap_int (int_of_string_opt (implode (List.rev str_acc)))))::acc)
  and
        read_oct cs acc str_acc = match cs with
        | '0'..'7' as c::cs -> read_oct cs acc (c::str_acc)
        | cs -> (print_endline (implode (List.rev str_acc)) ; tokenize cs ((INT (unwrap_int (int_of_string_opt (implode (List.rev str_acc)))))::acc))
  and
        read_bin cs acc str_acc = match cs with
        | '0'..'1' as c::cs -> read_bin cs acc (c::str_acc)
        | cs -> tokenize cs ((INT (unwrap_int (int_of_string_opt (implode (List.rev str_acc)))))::acc)
  and
        read_hex cs acc str_acc = match cs with
        | '0'..'9'|'A'..'F'|'a'..'f' as c::cs -> read_hex cs acc (c::str_acc)
        | cs -> tokenize cs ((INT (unwrap_int (int_of_string_opt (implode (List.rev str_acc)))))::acc)
  and 
        read_string cs acc str_acc = match cs with
        | '\\'::'\"'::cs -> read_string cs acc ((List.rev ['\\' ; '\"']) @ str_acc)
        | '\"'::cs -> tokenize cs ((STRING (implode (List.rev str_acc)))::acc)
        | c::cs -> read_string cs acc (c::str_acc)
        | [] -> tokenize [] ((ILLEGAL (implode (List.rev str_acc)))::acc)
  and
        read_ident chars acc str_acc = match chars with
        | [] -> tokenize [] ((ILLEGAL (implode (List.rev str_acc)))::acc)
        | '@'|'#'|'$' as c::cs -> tokenize cs (ILLEGAL (implode (List.rev (c::str_acc)))::acc)
        | ('!'|'%'|'^'|'&'|'*'|'('|')'|'{'
                |'}'|'['|']'|'/'|'?'|';'
                |'<'|'>'|'+'|'-'|'='|','
                |' '|'\t'|'\n')
                ::_ -> tokenize chars (IDENT (implode (List.rev str_acc))::acc)
        | c::cs -> read_ident cs acc (c::str_acc)

in List.rev (tokenize (explode str) []);;

let tok_to_string tok = match tok with
        | EOF -> "EOF"
        | IDENT x -> "IDENT(\""^x^"\")"
        | (ILLEGAL x) -> "ILLEGAL(\""^x^"\")"
        | (INT x) -> "INT("^(string_of_int x)^")"
        | (FLOAT x) -> "FLOAT("^(string_of_float x)^")"
        | (CHAR x) -> "CHAR("^(implode [x])^")"
        | (STRING x) -> "STRING(\""^x^"\")"
        | (LPAREN) -> "LPAREN"
        | (RPAREN) -> "RPAREN"
        | (LBRACE) -> "LBRACE"
        | (RBRACE) -> "RBRACE"
        | (LBRACKET) -> "LBRACKET"
        | (RBRACKET) -> "RBRACKET"
        | (COMMA) -> "COMMA"
        | (SEMICOLON) -> "SEMICOLON"
        | (COLON) -> "COLON"
        | (ASSIGN) -> "ASSIGN"
        | (PLUS) -> "PLUS"
        | (ASSIGN_PLUS) -> "ASSIGN_PLUS"
        | (MINUS) -> "MINUS"
        | (ASSIGN_MINUS) -> "ASSIGN_MINUS"
        | (STAR) -> "STAR"
        | (ASSIGN_MUL) -> "ASSIGN_MUL"
        | (DIV) -> "DIV"
        | (ASSIGN_DIV) -> "ASSIGN_DIV"
        | (EQ) -> "EQ"
        | (NEQ) -> "NEQ"
        | (NOT) -> "NOT"
        | (GT) -> "GT"
        | (LT) -> "LT"
        | (GTE) -> "GTE"
        | (LTE) -> "LTE"
        | (AND) -> "AND"
        | (BWAND) -> "BWAND"
        | (ASSIGN_BWAND) -> "ASSIGN_BWAND"
        | (OR) -> "OR"
        | (BWOR) -> "BWOR"
        | (ASSIGN_BWOR) -> "ASSIGN_BWOR"
        | (BWXOR) -> "BWXOR"
        | (ASSIGN_BWXOR) -> "ASSIGN_BWXOR"
        | (MOD) -> "MOD"
        | (ASSIGN_MOD) -> "ASSIGN_MOD"
        | (BWOCOMP) -> "BWOCOMP"
        | (BWLSHIFT) -> "BWLSHIFT"
        | (ASSIGN_BWLSHIFT) -> "ASSIGN_BWLSHIFT"
        | (BWRSHIFT) -> "BWRSHIFT"
        | (ASSIGN_BWRSHIFT) -> "ASSIGN_BWRSHIFT"
        | (INC) -> "INC"
        | (DEC) -> "DEC"
        | (POUND) -> "POUND"
        | (SIZEOF) -> "SIZEOF"
        | (RETURN) -> "RETURN"
        | (TYPEDEF) -> "TYPEDEF"
        | (UNION) -> "UNION"
        | (CONST) -> "CONST"
        | (FOR) -> "FOR"
        | (WHILE) -> "WHILE"
        | (DO) -> "DO"
        | (UNSIGNED) -> "UNSIGNED"
        | (INTTYPE) -> "INTTYPE"
        | (FLOATTYPE) -> "FLOATTYPE"
        | (CHARTYPE) -> "CHARTYPE"
        | (STRUCTTYPE) -> "STRUCTTYPE"
        | (ENUMTYPE) -> "ENUMTYPE";;

let rec pp_list toks = match toks with
        | [] -> ()
        | t::ts -> (
        print_endline (tok_to_string t);
            pp_list ts
);;
