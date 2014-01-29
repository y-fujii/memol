(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

{
    open Parser

    let lexer = ref (fun _ -> EOF)
    let lex = fun buf -> !lexer buf

    exception TokenError
}

let label = ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']*

rule outer = parse
    | [' ' '\t' '\r' '\n']      { outer lexbuf }
    | '#' [^ '\n']*             { outer lexbuf }
    | '{'                       { lexer := inner; LBRACE }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | '['                       { LBRACKET }
    | ']'                       { RBRACKET }
    | '/'                       { SLASH }
    | '='                       { EQUAL }
    | '*'                       { STAR }
    | "score"                   { SCORE }
    | "value"                   { VALUE }
    | ['0'-'9']+                { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
    | label                     { LABEL(Lexing.lexeme lexbuf) }
    | eof                       { EOF }
    | _                         { raise TokenError }

and inner = parse
    | [' ' '\t' '\r' '\n']      { inner lexbuf }
    | '#' [^ '\n']*             { inner lexbuf }
    | '}'                       { lexer := outer; RBRACE }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | '['                       { LBRACKET }
    | ']'                       { RBRACKET }
    | '/'                       { SLASH }
    | '<'                       { LESS }
    | '>'                       { GREATER }
    | '$'                       { DOLLAR }
    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '^'                       { HAT }
    | '|'                       { VERT }
    | '_'                       { UNDER }
    | '.'                       { DOT }
    | ['a'-'z']                 { SMALLALPHA((Lexing.lexeme lexbuf).[0]) }
    | ['A'-'Z']                 { LARGEALPHA(Char.lowercase (Lexing.lexeme lexbuf).[0]) }
    | ['0'-'9']                 { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
    | _                         { raise TokenError }

{
    let () = (lexer := outer)
}
