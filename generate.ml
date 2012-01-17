(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc
open Ast


exception Error

module TieMap = Map.Make(struct
    type t = (int * char * int)
    let compare = compare
end)

module State = struct
    type t = {
        prevNote: int * char * int;
        prevTree: NoteSyntax.t;
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


let rec generateSeq = (fun info state acc tree ->
    (match tree with
        | NoteSyntax.Lower(sym1, chr1) ->
            let (oct0, sym0, chr0) = state.State.prevNote in
            let oct1 = oct0 - if (sym1, chr1) <= (sym0, chr0) then 0 else 1 in
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

        | NoteSyntax.Upper(sym1, chr1) ->
            let (oct0, sym0, chr0) = state.State.prevNote in
            let oct1 = oct0 + if (sym1, chr1) >= (sym0, chr0) then 0 else 1 in
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

        | NoteSyntax.Rest ->
            (state, acc)

        | NoteSyntax.Repeat ->
            generateSeq info state acc state.State.prevTree

        | NoteSyntax.Chord([]) ->
            (state, acc)

        | NoteSyntax.Chord(tree1 :: trees) ->
            let (state, acc) = generateSeq info state acc tree1 in
            let (_, tiedNote, acc) = trees |> List.fold_left (fun (prevNote, tiedNote, acc) tree ->
                let state = { state with State.prevNote = prevNote } in
                let (state, acc) = generateSeq info state acc tree in
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

        | NoteSyntax.Group(trees) ->
            let ndiv = trees |> List.fold_left (fun n (_, w) -> n + w) 0 in
            let span = Num.((info.Info.timeEnd -/ info.Info.timeBgn) // (num_of_int ndiv)) in
            let trees = if info.Info.tie then
                trees |> List.applyLst (fun (tree, w) -> (NoteSyntax.Tie(tree), w))
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
                let (state, acc) = generateSeq info state acc tree in
                (t1, state, acc)
            ) (info.Info.timeBgn, state, acc) in
            (state, acc)

        | NoteSyntax.Octave(i) ->
            let (oct, sym, chr) = state.State.prevNote in
            let state = { state with State.
                prevNote = (oct + i, sym, chr)
            } in
            (state, acc)
     
        | NoteSyntax.Tie(tree) ->
            let info = { info with Info.tie = true } in
            generateSeq info state acc tree
    )
)

let sortSeq =
    List.sort (fun (xt0, _, xoct, xsym, xchr) (yt0, _, yoct, ysym, ychr) ->
        let c = Num.compare_num xt0 yt0 in
        if c != 0 then
            c
        else
            compare (xoct, xsym, xchr) (yoct, ysym, ychr)
    )

let printSeq = (fun seq ->
    seq |> sortSeq |> List.iter (fun (t0, t1, oct, sym, chr) ->
        let sch = if chr >= 0 then
            String.make chr '+'
        else
            String.make (-chr) '-'
        in
        Printf.printf "[%4d %4d] %+d %c%s\n" (Num.int_of_num t0) (Num.int_of_num t1) oct sym sch
    )
)
