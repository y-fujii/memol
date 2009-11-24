(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


let getOptions = (fun () ->
    let src = ref None in
    let dst = ref None in
    Arg.parse
        [
            ("-o", Arg.String(fun arg -> dst := Some(arg)), "\tSpecify the output file name.");
        ]
        (fun arg ->
            (match !src with
                | Some(_) -> raise (Failure "Invalid arguments.")
                | None -> src := Some(arg)
            )
        )
        "Usage:"
    ;
    (!dst, !src)
)

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

let main = (fun () ->
    let (odst, osrc) = getOptions() in
    let src = (match osrc with
        | Some(src) -> src
        | _ -> raise (Failure "")
    ) in

    let ast = src |> openInSafe (fun ich ->
        let buf = Lexing.from_channel ich in
        try
            Parser.top Lexer.lex buf
        with Lexer.TokenError | Parsing.Parse_error as err -> (
            let line = buf.Lexing.lex_buffer |> ExtStr.count 0 buf.Lexing.lex_curr_pos '\n' in
            let msg = Printf.sprintf "Syntax error at line #%d." (line + 1) in
            raise (Failure msg)
        )
    ) in

    let note1st = (match ast with
        | [Ast.Top.Def(_, Ast.PhraseSyntax.Score(_, note :: _))] -> note
        | _ -> raise (Failure "aa")
    ) in
    let note1stSimple = (match Simplify.simplifyNote note1st { Simplify.State.
        octave = 4;
        symbol = 'a';
        half = 0;
        note = None;
    } with
        | (Some(n), _) -> n
        | _ -> raise (Failure "")
    ) in
    print_string ((Pprint.dumpNoteSimple note1stSimple) ^ "\n")
)


(* Let's start. *)
;; main ()
