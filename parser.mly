/* by y.fujii <y-fujii at mimosa-pudica.net>, public domain */

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET EQUAL LESS GREATER DOLLAR
%token PLUS MINUS UNDER SLASH HAT STAR VERT SCORE VALUE DOT
%token <int> INTEGER
%token <char> SMALLALPHA LARGEALPHA
%token <string> LABEL
%token EOF

%start top
%type <Ast.Top.t list> top

%%

top
    : LABEL EQUAL phrase top                { Top.Def($1, $3) :: $4 }
    |                                       { [] }

phrase
    : SLASH                                 { Phrase.Repeat }
    | LABEL	                                { Phrase.Variable($1) }
    | LBRACKET innerSequence RBRACKET       { Phrase.Sequence($2) }
    | LPAREN innerParallel RPAREN           { Phrase.Parallel($2) }
    | SCORE LBRACE innerScore RBRACE	    { Phrase.Score($3) }

innerSequence
    : phrase innerSequence                  { $1 :: $2 }
    |                                       { [] }

innerParallel
    : phrase STAR innerParallel             { ($1, true) :: $3 }
    | phrase innerParallel                  { ($1, false) :: $2 }
    |                                       { [] }

innerScore
    : innerGroup VERT innerScore            { Note.Group($1) :: $3 }
    | innerGroup                            { [ Note.Group($1) ] }

note
    : UNDER                                 { Note.Rest }
    | SLASH                                 { Note.Repeat }
    | DOT	                                { Note.RepeatTied }
    | SMALLALPHA keySig                     { Note.RelNote(-1, $1, $2) }
    | LARGEALPHA keySig                     { Note.RelNote(+1, $1, $2) }
    | LBRACKET innerGroup RBRACKET          { Note.Group($2) }
    | LPAREN innerChord RPAREN              { Note.Chord($2) }
    | note HAT                              { Note.Tie($1) }

octave
    : LESS                                  { Note.Octave(+1) }
    | GREATER                               { Note.Octave(-1) }

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
