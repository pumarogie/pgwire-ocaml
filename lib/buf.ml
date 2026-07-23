(* buf.ml — big-endian byte helpers: the foundation the whole protocol sits on.

   PostgreSQL's wire protocol is big-endian. A backend message is:
       tag(1 byte)  length(int32, counts itself + payload, NOT the tag)  payload
   We build outgoing messages into a string with [message], and pull integers /
   C-strings out of incoming payloads with the get_* cursor helpers. *)

(* --- reading (cursor over a string; caller tracks position) --- *)

let get_u8 s p = Char.code s.[p]

let get_int16 s p = (get_u8 s p lsl 8) lor get_u8 s (p + 1)

let get_int32 s p =
  (get_u8 s p lsl 24)
  lor (get_u8 s (p + 1) lsl 16)
  lor (get_u8 s (p + 2) lsl 8)
  lor get_u8 s (p + 3)

(* Null-terminated string starting at [p]; returns (value, position after \0). *)
let get_cstring s p =
  let e = String.index_from s p '\000' in
  (String.sub s p (e - p), e + 1)

(* --- writing --- *)

let add_int16 b n =
  let n = n land 0xffff in
  Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char b (Char.chr (n land 0xff))

let add_int32 b n =
  let n = n land 0xffffffff in
  Buffer.add_char b (Char.chr ((n lsr 24) land 0xff));
  Buffer.add_char b (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char b (Char.chr (n land 0xff))

let add_cstring b s =
  Buffer.add_string b s;
  Buffer.add_char b '\000'

(* Frame a payload as a tagged backend message: tag + int32(len) + payload. *)
let message tag payload =
  let b = Buffer.create (5 + String.length payload) in
  Buffer.add_char b tag;
  add_int32 b (4 + String.length payload);
  Buffer.add_string b payload;
  Buffer.contents b

(* self-check: run via `main test` *)
let demo () =
  let b = Buffer.create 8 in
  add_int32 b 196608;
  assert (get_int32 (Buffer.contents b) 0 = 196608);
  let b16 = Buffer.create 4 in
  add_int16 b16 (-1);
  assert (get_int16 (Buffer.contents b16) 0 = 0xffff);
  let m = message 'R' "\000\000\000\000" in
  assert (m.[0] = 'R');
  assert (get_int32 m 1 = 8);
  (* len counts itself(4) + payload(4) *)
  let s, p = get_cstring "hi\000rest" 0 in
  assert (s = "hi" && p = 3);
  print_endline "buf: ok"
