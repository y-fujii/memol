(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)


module Note = struct
    type t =
        | RelNote of int * char * int
        | Rest
        | Repeat
        | RepeatTied
        | Octave of int
        | Chord of t list
        | Group of (t * int) list
        | Tie of t
end

module Phrase = struct
    type t =
        | Repeat
        | Score of Note.t list
        | Variable of string
        | Parallel of (t * bool) list
        | Sequence of t list
end

module Top = struct
    type t =
        | Def of string * Phrase.t
end
