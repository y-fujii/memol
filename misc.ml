(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)


let (|>) = fun v f -> f v

let string_of_char = Printf.sprintf "%c"

module ExtStr = struct
    (* tailcall *)
    let rec count = (fun n0 n1 c str ->
        let rec loop = (fun cnt i ->
            if i >= n1 then
                cnt
            else
                let cnt = if str.[i] = c then cnt + 1 else cnt in
                loop cnt (i + 1)
        ) in
        loop 0 n0
    )
end

module ExtList = struct
    let getFirst = (fun xs ->
        (match xs with
            | [] ->
                raise (Failure "getFirst")
            | car :: cdr ->
                car
        )
    )

    let rec getLast = (fun xs ->
        (match xs with
            | [] ->
                raise (Failure "getLast")
            | car :: [] ->
                car
            | car :: cdr ->
                getLast cdr
        )
    )

    let applyFirst = (fun f xs ->
        (match xs with
            | [] ->
                raise (Failure "applyFirst")
            | car :: cdr ->
                (f car) :: cdr
        )
    )

    let rec applyLast = (fun f xs ->
        (match xs with
            | [] ->
                raise (Failure "applyLast")
            | car :: [] ->
                (f car) :: []
            | car :: cdr ->
                car :: (applyLast f cdr)
        )
    )

    let rec mapWithState = (fun f state xs ->
        (match xs with
            | [] ->
                (state, [])

            | x1 :: xs ->
                let (state1, x1) = f state x1 in
                let (stateL, xs) = mapWithState f state1 xs in
                (stateL, x1 :: xs)
        )
    )
end


module ExtHash = struct
    let merge = (fun xs ys ->
        let tbl = Hashtbl.copy xs in
        ys |> Hashtbl.iter (fun k v ->
            Hashtbl.replace tbl k v
        );
        tbl
    )
end
