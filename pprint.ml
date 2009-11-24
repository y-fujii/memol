(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)
 
open Misc
open Ast


let rec dumpNoteSimple = (fun note ->
    (match note with
        | NoteSimple.Note(oct, sym, half, tied) ->
            let sHalf = if half > 0 then
                String.make half '+'
            else
                String.make (-half) '-'
            in
            let sTied = if tied then "^" else "" in
            (string_of_int oct) ^ (string_of_char sym) ^ sHalf ^ sTied

        | NoteSimple.Rest ->
            "_"

        | NoteSimple.Chord(notes) ->
            let sNotes = notes |> List.fold_left (fun str note ->
                str ^ " " ^ (dumpNoteSimple note)
            ) "" in
            "(" ^ sNotes ^ " )"

        | NoteSimple.Group(notes) ->
            let sNotes = notes |> List.fold_left (fun str (note, fact) ->
                let sFact = if fact > 1 then
                    string_of_int fact
                else
                    ""
                in
                str ^ " " ^ sFact ^ (dumpNoteSimple note)
            ) "" in
            "[" ^ sNotes ^ " ]"
    )
)

(*
let rec pprintPhrase = (fun prefix tree ->
    (match tree with
        | RepeatPhrase ->
            Printf.printf "%s/\n" prefix

        | Score((numr, denm), notes) -> begin
            Printf.printf "%sscore %d/%d {\n" prefix numr denm;
            List.iter (pprintNote ("\t" ^ prefix)) notes;
            Printf.printf "%s}\n" prefix;
        end
                
        | Parallel(phrase) -> begin
            Printf.printf "%sparallel [\n" prefix;
            List.iter (pprintPhrase ("\t" ^ prefix)) phrase;
            Printf.printf "%s]\n" prefix;
        end
            
        | Sequence(phrase) -> begin
            Printf.printf "%ssequence [\n" prefix;
            List.iter (pprintPhrase ("\t" ^ prefix)) phrase;
            Printf.printf "%s]\n" prefix;
        end
    )
)

let rec pprintTop = (fun prefix tree ->
    (match tree with
        | Macro(name, phrase) -> begin
                Printf.printf "%s%s =\n" prefix name;
                pprintPhrase ("\t" ^ prefix) phrase
        end
    )
)
*)
