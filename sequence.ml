(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


let sort =
    List.sort (fun (xt0, _, xoct, xsym, xchr) (yt0, _, yoct, ysym, ychr) ->
        if xt0 != yt0 then
            Num.compare_num xt0 yt0
        else
            compare (xoct, xsym, xchr) (yoct, ysym, ychr)
    )

let print = (fun seq ->
    seq |> sort |> List.iter (fun (t0, t1, oct, sym, chr) ->
        let sch = if chr >= 0 then
            String.make chr '+'
        else
            String.make (-chr) '-'
        in
        let open Num in
        Printf.printf "[%5d %5d] %4d %+d %c%s\n"
            (int_of_num t0)
            (int_of_num t1)
            (int_of_num (t1 -/ t0))
            oct sym sch
    )
)

let timeMap = (fun f ->
    List.map (fun (t0, t1, oct, sym, chr) ->
        (f t0, f t1, oct, sym, chr)
    )
)

let timeRange = (fun seq ->
    seq |> List.fold_left (fun (min_, max_) (t0, t1, _, _, _) ->
        let min_ = Num.min_num min_ t0 in
        let max_ = Num.max_num max_ t1 in
        (min_, max_)
    ) (Num.num_of_int max_int, Num.num_of_int min_int)
)

let symbolTbl = (
    let tbl = Hashtbl.create 0 in
    let add = Hashtbl.add tbl in
    add 'a' (-3);
    add 'b' (-1);
    add 'c'   0;
    add 'd'   2;
    add 'e'   4;
    add 'f'   5;
    add 'g'   7;
    tbl
)

let messages = (fun seq ->
    let dst = seq |> List.fold_left (fun dst (t0, t1, oct, sym, chr) ->
        let note = 60 + oct * 12 + (Hashtbl.find symbolTbl sym) + chr in
        let msg0 = (t0, 0, Midi.Event.NoteOn (note, 80)) in
        let msg1 = (t1, 0, Midi.Event.NoteOff(note, 80)) in
        msg0 :: msg1 :: dst
    ) [] in
    dst |> List.sort compare
 )
