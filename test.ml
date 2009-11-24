
let test = (fun name proc ->
    if proc () then
        print_string (name ^ " - Success.\n")
    else
        print_string (name ^ " - Failed.\n")
)

(* main *)
let () = (
    test "rational" (fun () ->
        let module R = Rational in
        (R.cmp (R.add (1, 2) (1, 2)) (R.sub (3, 2) (1, 2))) = 0
    );
)
