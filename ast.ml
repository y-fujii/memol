(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)


module NoteSyntax = struct
    type t =
        | Lower of char * int
        | Upper of char * int
        | Rest
        | Repeat
        | Octave of int
        | Chord of t list
        | Group of (t * int) list
        | Tie of t
end

module NoteSimple = struct
    type t =
        | Note of int * char * int * bool
        | Rest
        | Chord of t list
        | Group of (t * int) list
end

module PhraseSyntax = struct
    type t =
        | Repeat
        | Score of NoteSyntax.t list
        | Parallel of (t * bool) list
        | Sequence of t list
end

module PhraseSimple = struct
    type t =
        | Score of (int * int) * NoteSimple.t list
        | Parallel of t list
        | Sequence of t list
end

module Top = struct
    type t =
        | Def of string * PhraseSyntax.t
end
