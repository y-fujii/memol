(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

type t = (int * int)


let add = (fun (numr0, denm0) (numr1, denm1) ->
    if denm0 mod denm1 = 0 then
        (numr0 + numr1 * (denm0 / denm1), denm0)
    else if denm1 mod denm0 = 0 then
        (numr1 + numr0 * (denm1 / denm0), denm1)
    else
        (numr0 * denm1 + numr1 * denm0, denm0 * denm1)
)

let sub = (fun (numr0, denm0) (numr1, denm1) ->
    if denm0 mod denm1 = 0 then
        (numr0 - numr1 * (denm0 / denm1), denm0)
    else if denm1 mod denm0 = 0 then
        (numr1 - numr0 * (denm1 / denm0), denm1)
    else
        (numr0 * denm1 - numr1 * denm0, denm0 * denm1)
)

let mul = (fun (numr0, denm0) (numr1, denm1) ->
    (numr0 * numr1, denm0 * denm1)
)

let div = (fun (numr0, denm0) (numr1, denm1) ->
    (numr0 * denm1, denm0 * numr1)
)

let cmp = (fun (numr0, denm0) (numr1, denm1) ->
    compare (numr0 * denm1) (numr1 * denm0)
)

let toInt = (fun (numr, denm) ->
    numr / denm
)
