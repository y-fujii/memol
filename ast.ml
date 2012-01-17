(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)


module NoteSyntax = struct
    type t =
        | RelNote of int * char * int
        | Rest
        | Repeat
        | Octave of int
        | Chord of t list
        | Group of (t * int) list
        | Tie of t
end

module PhraseSyntax = struct
    type t =
        | Repeat
        | Score of NoteSyntax.t list
        | Parallel of (t * bool) list
        | Sequence of t list
end

module Top = struct
    type t =
        | Def of string * PhraseSyntax.t
end
