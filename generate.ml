(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


exception Error

module DefMap = Map.Make(struct
    type t = string
    let compare = String.compare
end)

module TieMap = Map.Make(struct
    type t = (int * char * int)
    let compare = compare
end)

module State = struct
    type t = {
        prev: int * char * int;
        ties: Num.num TieMap.t;
    }
end

module Info = struct
    type t = {
        timeBgn: Num.num;
        timeEnd: Num.num;
        tie: bool;
    }
end


let rec elimRepeat = (fun prev tree ->
    (match tree with
        | Ast.Note.RelNote(_, _, _) -> (tree, tree)
        | Ast.Note.Rest             -> (tree, tree)
        | Ast.Note.Repeat           -> (prev, prev)
        | Ast.Note.Octave(_)        -> (prev, tree)
        | Ast.Note.Tie(tree) ->
            let (prev, tree) = elimRepeat prev tree in
            let tree = Ast.Note.Tie(tree) in
            (prev, tree)
        | Ast.Note.Chord(trees) ->
            let (_, trees) = trees |> List.fold_left (fun (prev, acc) tree ->
                let (prev, tree) = elimRepeat prev tree in
                (prev, tree :: acc)
            ) (prev, []) in
            let tree = Ast.Note.Chord(List.rev trees) in
            (tree, tree)
        | Ast.Note.Group(trees) ->
            let (prev, trees) = trees |> List.fold_left (fun (prev, acc) (tree, w)->
                let (prev, tree) = elimRepeat prev tree in
                (prev, (tree, w) :: acc)
            ) (prev, []) in
            let tree = Ast.Note.Group(List.rev trees) in
            (prev, tree)
    )
)

let rec generateNote = (fun info state acc tree ->
    (match tree with
        | Ast.Note.RelNote(dir, sym1, chr1) ->
            let (oct0, sym0, chr0) = state.State.prev in
            let oct1 = oct0 + (match dir with
                | -1 -> if (sym1, chr1) <= (sym0, chr0) then 0 else -1
                | +1 -> if (sym1, chr1) >= (sym0, chr0) then 0 else +1
                | _  -> assert false
            ) in
            let timeBgn = (match
                TieMap.findOpt (oct1, sym1, chr1) state.State.ties
            with
                | Some(t) -> t
                | None    -> info.Info.timeBgn
            ) in
            let acc = if info.Info.tie then
                acc
            else
                (timeBgn, info.Info.timeEnd, oct1, sym1, chr1) :: acc
            in
            let ties = if info.Info.tie then
                TieMap.singleton (oct1, sym1, chr1) timeBgn
            else
                TieMap.empty
            in
            let state = { State.
                prev = (oct1, sym1, chr1);
                ties = ties;
            } in
            (state, acc)

        | Ast.Note.Repeat ->
            assert false

        | Ast.Note.Rest
        | Ast.Note.Chord([]) ->
            let state = { state with State.ties = TieMap.empty } in
            (state, acc)

        | Ast.Note.Chord(tree1 :: trees) ->
            let (state1, acc) = generateNote info state acc tree1 in
            let (_, ties, acc) = trees |> List.fold_left (fun (prev, ties, acc) tree ->
                let stateN = { state with State.prev = prev } in
                let (stateN, acc) = generateNote info stateN acc tree in
                let ties = TieMap.merge (fun _ x y ->
                    (match (x, y) with
                        | (Some(_), Some(_)) -> raise Error
                        | (Some(x), None   ) -> Some(x)
                        | (None   , Some(y)) -> Some(y)
                        | (None   , None   ) -> None
                    )
                ) ties stateN.State.ties in
                (stateN.State.prev, ties, acc)
            ) (state1.State.prev, state1.State.ties, acc) in
            let state = { State.
                prev = state1.State.prev;
                ties = ties;
            } in
            (state, acc)

        | Ast.Note.Group(trees) ->
            let ndiv = trees |> List.fold_left (fun n (_, w) -> n + w) 0 in
            if ndiv = 0 then raise Error else
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
            let (oct, sym, chr) = state.State.prev in
            let state = { state with State.
                prev = (oct + i, sym, chr)
            } in
            (state, acc)
     
        | Ast.Note.Tie(tree) ->
            let info = { info with Info.tie = true } in
            generateNote info state acc tree
    )
)

let rec generatePhrase = (fun defs i acc tree ->
    (match tree with
        | Ast.Phrase.Score(notes) ->
            let state = { State.
                prev = (0, 'a', 0);
                ties = TieMap.empty;
            } in
            let (i, _, _, acc) = notes |> List.fold_left (fun (i, prev, state, acc) note ->
                let info = { Info.
                    timeBgn = i;
                    timeEnd = Num.(i +/ (num_of_int 1));
                    tie = false;
                } in
                let (prev, note) = elimRepeat prev note in
                let (state, acc) = generateNote info state acc note in
                (Num.(i +/ (num_of_int 1)), prev, state, acc)
            ) (i, Ast.Note.Rest, state, acc) in
            (i, acc)

        | Ast.Phrase.Repeat ->
            raise (Failure "NYI")

        | Ast.Phrase.Variable(name) ->
            (match DefMap.findOpt name defs with
                | Some(seq) ->
                    (* XXX *)
                    let seq = seq |> Sequence.timeMap (fun t -> Num.(i +/ t)) in
                    let (_, i) = Sequence.timeRange seq in
                    (i, seq @ acc)
                | None ->
                    raise Error
            )

        | Ast.Phrase.Sequence(trees) ->
            trees |> List.fold_left (fun (i, acc) tree ->
                generatePhrase defs i acc tree
            ) (i, acc)

        | Ast.Phrase.Parallel(trees) ->
            let (fi, acc) = trees |> List.fold_left (fun (j, acc) (tree, v) ->
                if not v then
                    let (k, acc) = generatePhrase defs i acc tree in
                    (Num.max_num j k, acc)
                else
                    (j, acc)
            ) (i, acc) in
            let (vi, acc) = trees |> List.fold_left (fun (j, acc) (tree, v) ->
                if v then
                    let rec loop = (fun k acc ->
                        if k < fi then
                            let (k, acc) = generatePhrase defs k acc tree in
                            loop k acc
                        else
                            (k, acc)
                    ) in
                    let (k, acc) = loop i acc in
                    (Num.max_num j k, acc)
                else
                    (j, acc)
            ) (i, acc) in
            (Num.max_num fi vi, acc)
    )
)

let generate = (fun trees ->
    trees |> List.fold_left (fun defs tree ->
        (match tree with
            | Ast.Top.Def(name, tree) ->
                let (_, seq) = generatePhrase defs (Num.num_of_int 0) [] tree in
                DefMap.add name seq defs
        )
    ) DefMap.empty
)
