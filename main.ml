(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


let getOptions = (fun () ->
    let src = ref None in
    let dst = ref None in
    let useJack = ref false in
    Arg.parse
        [
            ("-o",
                Arg.String(fun arg -> dst := Some(arg)),
                "\tSpecify the output file name."
            );
            ("-j",
                Arg.Unit(fun () -> useJack := true),
                "\tPlay with Jack."
            );
        ]
        (fun arg ->
            (match !src with
                | Some(_) -> raise (Failure "Invalid arguments.")
                | None -> src := Some(arg)
            )
        )
        "Usage:"
    ;
    (!dst, !src, !useJack)
)

let main = (fun () ->
    let (_, osrc, useJack) = getOptions() in
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

    let defs = Generate.generate ast in
    let seq = (match Generate.DefMap.findOpt "main" defs with
        | Some(seq) -> seq
        | None -> raise (Failure "An entry point is not found.")
    ) in
    let seq = seq |> Sequence.timeMap (fun t -> Num.((num_of_int (960 * 2)) */ t)) in
    Sequence.print seq;
    flush stdout;

    if useJack then (
        let msgs = seq |> Sequence.messages in
        let jack = Jack.init "memol" in
        try (
            Jack.connect jack "memol:out" "midi-monitor:input";
            Jack.connect jack "memol:out" "LinuxSampler:midi_in_0";
            Jack.stop jack;
            Jack.seek jack 0 960;
            Jack.data jack msgs 960;
            Jack.play jack;
            Unix.sleep 3600;
        )
        with _ -> (
            Jack.stop jack;
            Jack.term jack;
        )
    )
    else ()
)


(* Let's start. *)
;; main ()
