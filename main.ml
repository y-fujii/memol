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
        with Lexer.TokenError | Parsing.Parse_error -> (
            let text = String.sub buf.Lexing.lex_buffer 0 buf.Lexing.lex_curr_pos in
            let nLine = text |> String.fold (fun n c ->
                 if c = '\n' then n + 1 else n 
            ) 1 in
            let msg = Printf.sprintf "Syntax error at line #%d." nLine in
            raise (Failure msg)
        )
    ) in

    let note1st = (match ast with
        | [Ast.Top.Def(_, Ast.PhraseSyntax.Score(note :: _))] -> note
        | _ -> raise (Failure "")
    ) in
    let info = { Generate.Info.
        timeBgn = Num.num_of_int 0;
        timeEnd = Num.num_of_int 240;
        tie = false;
    } in
    let state = { Generate.State.
        prevNote = (0, 'a', 0);
        prevTree = Ast.NoteSyntax.Rest;
        tiedNote = Generate.TieMap.empty;
    } in
    let (_, seq) = Generate.generateSeq info state [] note1st in
    Generate.printSeq seq
)


(* Let's start. *)
;; main ()
