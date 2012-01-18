(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


exception Error

module TieMap = Map.Make(struct
    type t = (int * char * int)
    let compare = compare
end)

module State = struct
    type t = {
        prevNote: int * char * int;
        prevTree: Ast.Note.t;
        tiedNote: Num.num TieMap.t;
    }
end

module Info = struct
    type t = {
        timeBgn: Num.num;
        timeEnd: Num.num;
        tie: bool;
    }
end


let rec generateNote = (fun info state acc tree ->
    (match tree with
        | Ast.Note.RelNote(dir, sym1, chr1) ->
            let (oct0, sym0, chr0) = state.State.prevNote in
            let oct1 = oct0 + (match dir with
                | -1 -> if (sym1, chr1) <= (sym0, chr0) then 0 else -1
                | +1 -> if (sym1, chr1) >= (sym0, chr0) then 0 else +1
                | _  -> assert false
            ) in
            let acc = if info.Info.tie then
                acc
            else
                let timeBgn = (match TieMap.findOpt (oct1, sym1, chr1) state.State.tiedNote with
                    | Some(t) -> t
                    | None    -> info.Info.timeBgn
                ) in
                (timeBgn, info.Info.timeEnd, oct1, sym1, chr1) :: acc
            in
            let tiedNote = if info.Info.tie then
                TieMap.singleton (oct1, sym1, chr1) info.Info.timeBgn
            else
                TieMap.empty
            in
            let state = { State.
                prevNote = (oct1, sym1, chr1);
                prevTree = tree;
                tiedNote = tiedNote;
            } in
            (state, acc)

        | Ast.Note.Repeat ->
            generateNote info state acc state.State.prevTree

        | Ast.Note.Rest
        | Ast.Note.Chord([]) ->
            let state = { state with State.tiedNote = TieMap.empty } in
            (state, acc)

        | Ast.Note.Chord(tree1 :: trees) ->
            let (state, acc) = generateNote info state acc tree1 in
            let (_, tiedNote, acc) = trees |> List.fold_left (fun (prevNote, tiedNote, acc) tree ->
                let state = { state with State.prevNote = prevNote } in
                let (state, acc) = generateNote info state acc tree in
                let tiedNote = TieMap.merge (fun k x y ->
                    (match (x, y) with
                        | (Some(x), Some(y)) -> raise Error
                        | (Some(x), None   ) -> Some(x)
                        | (None   , Some(y)) -> Some(y)
                        | (None   , None   ) -> None
                    )
                ) tiedNote state.State.tiedNote in
                (state.State.prevNote, tiedNote, acc)
            ) (state.State.prevNote, state.State.tiedNote, acc) in
            let state = { state with State.
                prevTree = tree;
                tiedNote = tiedNote;
            } in
            (state, acc)

        | Ast.Note.Group(trees) ->
            let ndiv = trees |> List.fold_left (fun n (_, w) -> n + w) 0 in
            let span = Num.(Info.(info.timeEnd -/ info.timeBgn) // (num_of_int ndiv)) in
            let trees = if info.Info.tie then
                (* XXX: Octave *)
                trees |> List.applyLst (fun (tree, w) -> (Ast.Note.Tie(tree), w))
            else
                trees
            in
            let (_, state, acc) = trees |> List.fold_left (fun (t0, state, acc) (tree, w) ->
                let t1 = Num.(t0 +/ (num_of_int w) */ span) in
                let info = { Info.
                    timeBgn = t0;
                    timeEnd = t1;
                    tie = false;
                } in
                let (state, acc) = generateNote info state acc tree in
                (t1, state, acc)
            ) (info.Info.timeBgn, state, acc) in
            (state, acc)

        | Ast.Note.Octave(i) ->
            let (oct, sym, chr) = state.State.prevNote in
            let state = { state with State.
                prevNote = (oct + i, sym, chr)
            } in
            (state, acc)
     
        | Ast.Note.Tie(tree) ->
            let info = { info with Info.tie = true } in
            generateNote info state acc tree
    )
)

let rec generatePhrase = (fun i acc tree ->
    (match tree with
        | Ast.Phrase.Score(notes) ->
            let state = { State.
                prevNote = (0, 'a', 0);
                prevTree = Ast.Note.Rest;
                tiedNote = TieMap.empty;
            } in
            let (i, _, acc) = notes |> List.fold_left (fun (i, state, acc) note ->
                let info = { Info.
                    timeBgn = Num.num_of_int  i;
                    timeEnd = Num.num_of_int (i + 1);
                    tie = false;
                } in
                let (state, acc) = generateNote info state acc note in
                (i + 1, state, acc)
            ) (i, state, acc) in
            (i, acc)

        | Ast.Phrase.Repeat ->
            raise (Failure "NYI")

        | Ast.Phrase.Sequence(trees) ->
            trees |> List.fold_left (fun (i, acc) tree ->
                generatePhrase i acc tree
            ) (i, acc)

        | Ast.Phrase.Parallel(trees) ->
            let (fi, acc) = trees |> List.fold_left (fun (j, acc) (tree, v) ->
                if not v then
                    let (k, acc) = generatePhrase i acc tree in
                    (max j k, acc)
                else
                    (j, acc)
            ) (i, acc) in
            let (vi, acc) = trees |> List.fold_left (fun (j, acc) (tree, v) ->
                if v then
                    let rec loop = (fun k acc ->
                        if k < fi then
                            let (k, acc) = generatePhrase k acc tree in
                            loop k acc
                        else
                            (k, acc)
                    ) in
                    let (k, acc) = loop i acc in
                    (max j k, acc)
                else
                    (j, acc)
            ) (i, acc) in
            (max fi vi, acc)
    )
)
