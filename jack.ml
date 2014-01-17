open Misc

type t

external cJackInit: string -> t = "jackInit"
external cJackTerm: t -> unit = "jackTerm"
external cJackPlay: t -> unit = "jackPlay"
external cJackStop: t -> unit = "jackStop"
external cJackSeek: t -> int -> int -> unit = "jackSeek"
external cJackConnect: t -> string -> string -> unit = "jackConnect"
external cJackData: t -> (int * string) list -> int -> unit = "jackData"

let init = cJackInit
let term = cJackTerm
let play = cJackPlay
let stop = cJackStop
let seek = cJackSeek
let connect = cJackConnect

let data = (fun jack msgs base ->
    let buf = Buffer.create 4 in
    let midi = msgs |> List.map (fun (t, ch, ev) ->
        Buffer.clear buf;
        Smf.dumpMidi buf ch ev;
        (Num.int_of_num t, Buffer.contents buf)
    ) in
    cJackData jack midi base
)
