(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc
open Ast


module State = struct
    type t = {
        octave: int;
        symbol: char;
        half: int;
        note: NoteSyntax.t option;
    }
end


let rec mapFilterWithState = (fun f state xs ->
    (match xs with
        | [] ->
            (state, [])

        | x1 :: xs ->
            let (state1, x1) = f state x1 in
            let (stateL, xs) = mapFilterWithState f state1 xs in
            let xs' = (match x1 with
                | Some(x1') -> x1' :: xs
                | None -> xs
            ) in
            (stateL, xs')
    )
)

let rec simplifyNote = (fun note state ->
    (match note with
        | NoteSyntax.Lower(sym, half) ->
            let octave =
                if (sym, half) <= (state.State.symbol, state.State.half) then
                    state.State.octave
                else
                    state.State.octave - 1 
            in
            let dstNote = NoteSimple.Note(octave, sym, half, false) in
            let dstState = { State.
                octave = octave;
                symbol = sym;
                half = half;
                note = Some(note);
            } in
            (Some(dstNote), dstState)

        | NoteSyntax.Upper(sym, half) ->
            let octave =
                if (sym, half) >= (state.State.symbol, state.State.half) then
                    state.State.octave
                else
                    state.State.octave + 1 
            in
            let dstNote = NoteSimple.Note(octave, sym, half, false) in
            let dstState = { State.
                octave = octave;
                symbol = sym;
                half = half;
                note = Some(note);
            } in
            (Some(dstNote), dstState)

        | NoteSyntax.Rest ->
            (Some(NoteSimple.Rest), { state with State.note = Some(note) })

        | NoteSyntax.Repeat ->
            (match state.State.note with
                | Some(note) ->
                    simplifyNote note state

                | None ->
                    raise (Failure "")
            )

        | NoteSyntax.Chord(note1 :: notes) ->
            let (note1, state1) = simplifyNote note1 state in
            let (stateL, notes) = notes |> mapFilterWithState (fun state note ->
                let (note, state) = simplifyNote note state in
                (state, note)
            ) state1 in
            let notes = (match note1 with
                | Some(note1) -> note1 :: notes
                | None -> notes
            ) in
            let state = { state1 with State.note = Some(note) } in
            (Some(NoteSimple.Chord(notes)), state)

        | NoteSyntax.Group(_ :: _ as notes) ->
            let (state, notes) = notes |> mapFilterWithState (fun state (note, fact) ->
                let (note, state) = simplifyNote note state in
                (state, (match note with
                    | Some(note) -> Some((note, fact))
                    | None -> None
                ))
            ) state in
            (Some(NoteSimple.Group(notes)), state)

        | NoteSyntax.Tie(NoteSyntax.Repeat) ->
            simplifyNote NoteSyntax.Repeat state

        | NoteSyntax.Tie(NoteSyntax.Rest) ->
            (Some(NoteSimple.Rest), state)

        | NoteSyntax.Tie(NoteSyntax.Chord(notes)) ->
            let notes = notes |> List.map (fun n ->
                NoteSyntax.Tie(n)
            ) in
            simplifyNote (NoteSyntax.Chord(notes)) state

        | NoteSyntax.Tie(NoteSyntax.Group(notes)) ->
            let notes = notes |> ExtList.applyLast (fun (n, f) ->
                (NoteSyntax.Tie(n), f)
            ) in
            simplifyNote (NoteSyntax.Group(notes)) state

        | NoteSyntax.Tie(NoteSyntax.Tie(note)) ->
            simplifyNote (NoteSyntax.Tie(note)) state

        | NoteSyntax.Octave(i) ->
            (None, { state with State.octave = state.State.octave + i })

        | NoteSyntax.Chord([])
        | NoteSyntax.Group([])
        | NoteSyntax.Tie(NoteSyntax.Octave(_)) ->
            raise (Failure "")
    )
)
