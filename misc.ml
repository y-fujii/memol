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

    (*
    let rec mapFilter = (fun f xs ->
        (match xs with
            | [] ->
                []
            | x1 :: xs ->
                let ys = mapFilter xs in
                (match f x1 with
                    | Some(y1) -> y1 :: ys
                    | None     -> ys
                )
        )
    )
    *)

    let rec mapFilterAcc = (fun f dst src ->
        (match src with
            | [] ->
                dst
            | x1 :: xs ->
                let dst = (match f x1 with
                    | Some(y1) -> y1 :: dst
                    | None     -> dst
                ) in
                mapFilterAcc f dst xs
        )
    )

    let mapFilter = (fun f xs ->
        xs |> mapFilterAcc f [] |> List.rev
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
        ys |> Hashtbl.iter (Hashtbl.replace tbl);
        tbl
    )
end

let openInSafe = (fun proc file ->
    let ich = open_in file in
    let result = try
        proc ich 
    with err -> (
        close_in_noerr ich;
        raise err
    ) in
    close_in ich;
    result
)
