(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


exception Error

type midiEvent =
    | NoteOff of int * int
    | NoteOn of int * int
    | Control of int * int
    | Tempo of int


let toStr = (fun src ->
    let dst = String.create (List.length src) in
    let rec loop = (fun i xs ->
        (match xs with
            | [] -> ()
            | x :: xs' -> (
                dst.[i] <- Char.chr x;
                loop (i + 1) xs'
            )
        )
    ) in
    loop 0 src;
    dst
)

let rec dumpIntBE = (fun l n ->
    if l = 0 then
        []
    else
        ((n lsr ((l - 1) * 8)) land 0xff) :: dumpIntBE n (l - 1)
)

let dumpTime = (fun t ->
    let rec calcByte = (fun n ->
        if t < 1 lsl (n + 1) then
            n
        else
            calcByte (n + 7)
    ) in
    let rec dump = (fun n ->
        let c = (t lsr (n - 7)) land 0x7f in
        if n = 7 then
            (c lor 0x00) :: []
        else
            (c lor 0x80) :: dump (n - 7)
    ) in
    dump (calcByte 7)
)
    
let dumpMidi = (fun ch ev ->
    (match ev with
        | NoteOff(note, vel) ->
            [ (0x80 lor ch); note; vel ]

        | NoteOn(note, vel) ->
            [ (0x90 lor ch); note; vel ]

        | Control(num, value) ->
            (match num with
                | num when num < 128 ->
                    [ (0xb0 lor ch); num; value ]

                | 128 -> (* program change *)
                    [ (0xb0 lor ch); value ]

                | 129 -> (* pitch bend *)
                    let lsb = value land 0x7f in
                    let msb = value lsr 7 in
                    (* Little endian! *)
                    [ (0xe0 lor ch); lsb; msb; ]

                | _ ->
                    raise Error
            )

        | Tempo(value) ->
            [ 0xff; 0x51; 0x03; value ]
    )
)

let dump = (fun events timeUnit ->
    let content = events |> ExtList.mapWithState (fun t0 (t1, ch, ev) ->
        let seq = dumpTime (t1 - t0) @ dumpMidi ch ev in
        (t1, seq)
    ) 0 |> snd |> List.flatten
    in
    "MThd" ^                           (* head chunk magic *)
    "\x00\x00\x00\x06" ^               (* chunk length *)
    "\x00\x00" ^                       (* format type *)
    "\x00\x01" ^                       (* # of tracks *)
    toStr (dumpIntBE 2 timeUnit) ^     (* unit of time *)
    "MTrk" ^                           (* track chunk magic *)
    toStr (dumpIntBE 4 (List.length content) @ content)
)

