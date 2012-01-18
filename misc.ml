(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)


let (|>) = fun v f -> f v

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

module String = struct
    include String

    let rec fold = (fun f acc str ->
        let n = length str in
        let rec loop = (fun i acc ->
            if i >= n then
                acc
            else
                loop (i + 1) (f acc str.[i])
        ) in
        loop 0 acc
    )
end

module List = struct
    include List

    let mapFilter = (fun f xs ->
        xs |> fold_left (fun acc x ->
            (match f x with
                | Some(y) -> y :: acc
                | None    -> acc
            )
        ) [] |> rev
    )

    let applyFst = (fun f xs ->
        (match xs with
            | []       -> []
            | x1 :: xs -> (f x1) :: xs
        )
    )
    
    let applyLst = (fun f xs ->
        xs |> rev |> applyFst f |> rev
    )
end

module Map = struct
    module type OrderedType = Map.OrderedType
    module type S = Map.S

    module Make(Ord: Map.OrderedType) = struct
        include Map.Make(Ord)

        let findOpt = (fun x m ->
            try
                Some(find x m)
            with Not_found ->
                None
        )
    end
end

module Buffer = struct
    include Buffer

    let addByte = (fun buf k ->
        add_char buf (Char.chr k)
    )

    let addInt16B = (fun buf k ->
        addByte buf (k lsr 8);
        addByte buf (k land 0xff);
    )

    let addInt32B = (fun buf k ->
        addByte buf  (k lsr 24);
        addByte buf ((k lsr 16) land 0xff);
        addByte buf ((k lsr  8) land 0xff);
        addByte buf ( k         land 0xff);
    )
end

module Num = struct
    include Num

    let min = (fun x y ->
        if x </ y then x else y
    )

    let max = (fun x y ->
        if x >/ y then x else y
    )
end
