(* by y.fujii <y-fujii at mimosa-pudica.net>, public domain *)

open Misc


exception Error

module Event = struct
    type t =
        | NoteOff of int * int
        | NoteOn  of int * int
        | Control of int * int
        | Tempo   of int
end


let dumpTime = (fun buf t ->
    let rec size = (fun n ->
        if t < 1 lsl (n * 7) then
            n
        else
            size (n + 1)
    ) in
    let rec dump = (fun n ->
        let c = (t lsr (n * 7)) land 0x7f in
        if n = 0 then
            Buffer.addByte buf (c lor 0x00)
        else (
            Buffer.addByte buf (c lor 0x80);
            dump (n - 1)
        )
    ) in
    dump ((size 1) - 1)
)
    
let dumpMidi = (fun buf ch ev ->
    (match ev with
        | Event.NoteOff(note, vel) ->
            List.iter (Buffer.addByte buf) [ (0x80 lor ch); note; vel ]

        | Event.NoteOn(note, vel) ->
            List.iter (Buffer.addByte buf) [ (0x90 lor ch); note; vel ]

        | Event.Control(num, value) ->
            (match num with
                | num when num < 128 ->
                    List.iter (Buffer.addByte buf) [ (0xb0 lor ch); num; value ]

                | 128 -> (* program change *)
                    List.iter (Buffer.addByte buf) [ (0xb0 lor ch); value ]

                | 129 -> (* pitch bend *)
                    let lsb = value land 0x7f in
                    let msb = value lsr 7 in
                    (* Little endian! *)
                    List.iter (Buffer.addByte buf) [ (0xe0 lor ch); lsb; msb ]

                | _ ->
                    raise Error
            )

        | Event.Tempo(value) ->
            List.iter (Buffer.addByte buf) [ 0xff; 0x51; 0x03; value ]
    )
)

let dump = (fun timeUnit buf events ->
    let content = Buffer.create 0 in
    let _ = events |> List.fold_left (fun t0 (t1, ch, ev) ->
        dumpTime content (t1 - t0);
        dumpMidi content ch ev;
        t1
    ) 0 in

    let open Buffer in
    add_string buf "MThd";                  (* head chunk magic *)
    addInt32B  buf 6;                       (* chunk length *)
    addInt16B  buf 0;                       (* format type *)
    addInt16B  buf 1;                       (* # of tracks *)
    addInt16B  buf timeUnit;                (* unit of time *)
    add_string buf "MTrk";                  (* track chunk magic *)
    addInt32B  buf ((length content) + 4);  (* chunk length *)
    add_buffer buf content;                 (* midi events *)
    add_string buf "\x00\xff\x2f\x00";      (* track end marker *)
)
