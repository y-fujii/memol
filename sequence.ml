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
        Printf.printf "[%4d %4d] %+d %c%s\n" (Num.int_of_num t0) (Num.int_of_num t1) oct sym sch
    )
)

let transTime = (fun a b ->
    List.map (fun (t0, t1, oct, sym, chr) ->
        (Num.(a */ t0 +/ b), Num.(a */ t1 +/ b), oct, sym, chr)
    )
)
