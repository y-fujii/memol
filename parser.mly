/* by y.fujii <y-fujii at mimosa-pudica.net>, public domain */

%{
    open Ast
%}

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token EQUAL
%token LESS
%token GREATER
%token DOLLAR
%token PLUS
%token MINUS
%token UNDER
%token SLASH
%token HAT
%token DOT
%token STAR
%token VERT
%token SCORE
%token VALUE
%token <int> INTEGER
%token <char> SMALLALPHA
%token <char> LARGEALPHA
%token <string> LABEL
%token EOF

%start top
%type <Ast.Top.t list> top

%%

top
    : LABEL EQUAL phrase top                { Top.Def($1, $3) :: $4 }
    |                                       { [] }

phrase
    : SLASH                                 { PhraseSyntax.Repeat }
    | LBRACKET innerSequence RBRACKET       { PhraseSyntax.Sequence($2) }
    | LPAREN innerParallel RPAREN           { PhraseSyntax.Parallel($2) }
    | SCORE LBRACE innerScore RBRACE	    { PhraseSyntax.Score($3) }

innerSequence
    : phrase innerSequence                  { $1 :: $2 }
    |                                       { [] }

innerParallel
    : phrase STAR innerParallel             { ($1, true) :: $3 }
    | phrase innerParallel                  { ($1, false) :: $2 }
    |                                       { [] }

innerScore
    : innerGroup VERT innerScore            { NoteSyntax.Group($1) :: $3 }
    | innerGroup                            { [ NoteSyntax.Group($1) ] }

note
    : UNDER                                 { NoteSyntax.Rest }
    | SLASH                                 { NoteSyntax.Repeat }
    | SMALLALPHA keySig                     { NoteSyntax.RelNote(-1, $1, $2) }
    | LARGEALPHA keySig                     { NoteSyntax.RelNote(+1, $1, $2) }
    | LBRACKET innerGroup RBRACKET          { NoteSyntax.Group($2) }
    | LPAREN innerChord RPAREN              { NoteSyntax.Chord($2) }
    | note HAT                              { NoteSyntax.Tie($1) }

octave
    : LESS                                  { NoteSyntax.Octave( 1) }
    | GREATER                               { NoteSyntax.Octave(-1) }

keySig
    : PLUS keySig                           { $2 + 1 }
    | MINUS keySig                          { $2 - 1 }
    |                                       { 0 }

innerGroup
    : note innerGroup                       { ($1,  1) :: $2 }
    | INTEGER note innerGroup               { ($2, $1) :: $3 }
    | octave innerGroup                     { ($1,  0) :: $2 }
    |                                       { [] }

innerChord
    : note innerChord                       { $1 :: $2 }
    | octave innerChord                     { $1 :: $2 }
    |                                       { [] }
