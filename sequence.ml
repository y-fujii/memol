(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


let sort =
    List.sort (fun (xt0, _, xoct, xsym, xchr) (yt0, _, yoct, ysym, ychr) ->
        let c = Num.compare_num xt0 yt0 in
        if c != 0 then
            c
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
