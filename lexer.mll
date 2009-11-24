(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

{
    open Parser

    let lexer = ref (fun buf -> DUMMY)
    let lex = fun buf -> !lexer buf

    exception TokenError
}

let label = ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']*

rule outer = parse
    | [' ' '\t' '\r' '\n']      { outer lexbuf }
    | '{'                       { lexer := inner; LBRACE }
    | '('	                    { LPAREN }
    | ')'	                    { RPAREN }
    | '['	                    { LBRACKET }
    | ']'	                    { RBRACKET }
    | '/'	                    { SLASH }
    | '='                       { EQUAL }
    | '*'                       { STAR }
    | "score"                   { SCORE }
    | "value"                   { VALUE }
    | ['0'-'9']+                { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
    | label                     { LABEL(Lexing.lexeme lexbuf) }
    | eof	                    { EOF }
    | _	                        { raise TokenError }

and inner = parse
    | [' ' '\t' '\r' '\n']      { inner lexbuf }
    | '}'                       { lexer := outer; RBRACE }
    | '('	                    { LPAREN }
    | ')'	                    { RPAREN }
    | '['	                    { LBRACKET }
    | ']'	                    { RBRACKET }
    | '/'	                    { SLASH }
    | '.'                       { DOT }
    | '>'                       { LESS }
    | '<'                       { GREATER }
    | '$'                       { DOLLAR }
    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '^'                       { HAT }
    | '|'                       { VERT }
    | '_'                       { UNDER }
    | ['a'-'z']                 { SMALLALPHA((Lexing.lexeme lexbuf).[0]) }
    | ['A'-'Z']                 { LARGEALPHA(Char.lowercase (Lexing.lexeme lexbuf).[0]) }
    | _	                        { raise TokenError }

{
    let () = (lexer := outer)
}
