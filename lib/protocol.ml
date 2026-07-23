(* protocol.ml — the byte layer: encode backend messages, decode frontend ones.

   Pure functions over strings/channels; no knowledge of SQL or storage. See the
   PostgreSQL "Frontend/Backend Protocol" (message formats) for the field order. *)

(* --- backend (server -> client) messages --- *)

let authentication_ok () =
  let b = Buffer.create 4 in
  Buf.add_int32 b 0;
  Buf.message 'R' (Buffer.contents b)

let parameter_status key value =
  let b = Buffer.create 32 in
  Buf.add_cstring b key;
  Buf.add_cstring b value;
  Buf.message 'S' (Buffer.contents b)

let backend_key_data pid secret =
  let b = Buffer.create 8 in
  Buf.add_int32 b pid;
  Buf.add_int32 b secret;
  Buf.message 'K' (Buffer.contents b)

let ready_for_query status =
  Buf.message 'Z' (String.make 1 status)

(* fields: (name, type_oid, type_size) *)
let row_description fields =
  let b = Buffer.create 64 in
  Buf.add_int16 b (List.length fields);
  List.iter
    (fun (name, oid, size) ->
      Buf.add_cstring b name;
      Buf.add_int32 b 0;
      (* table OID *)
      Buf.add_int16 b 0;
      (* column attribute number *)
      Buf.add_int32 b oid;
      Buf.add_int16 b size;
      Buf.add_int32 b (-1);
      (* type modifier *)
      Buf.add_int16 b 0
      (* format: 0 = text *))
    fields;
  Buf.message 'T' (Buffer.contents b)

(* one row; each column is Some rendered-text or None for NULL *)
let data_row cols =
  let b = Buffer.create 64 in
  Buf.add_int16 b (List.length cols);
  List.iter
    (function
      | None -> Buf.add_int32 b (-1)
      | Some s ->
        Buf.add_int32 b (String.length s);
        Buffer.add_string b s)
    cols;
  Buf.message 'D' (Buffer.contents b)

let command_complete tag =
  let b = Buffer.create 16 in
  Buf.add_cstring b tag;
  Buf.message 'C' (Buffer.contents b)

let empty_query_response () = Buf.message 'I' ""

let error_response code message =
  let b = Buffer.create 64 in
  Buffer.add_char b 'S';
  Buf.add_cstring b "ERROR";
  Buffer.add_char b 'C';
  Buf.add_cstring b code;
  Buffer.add_char b 'M';
  Buf.add_cstring b message;
  Buffer.add_char b '\000';
  (* terminator *)
  Buf.message 'E' (Buffer.contents b)

(* an informational NOTICE (same field layout as ErrorResponse, non-fatal) *)
let notice_response message =
  let b = Buffer.create 64 in
  Buffer.add_char b 'S';
  Buf.add_cstring b "NOTICE";
  Buffer.add_char b 'C';
  Buf.add_cstring b "00000";
  Buffer.add_char b 'M';
  Buf.add_cstring b message;
  Buffer.add_char b '\000';
  Buf.message 'N' (Buffer.contents b)

(* --- extended query protocol (server -> client) --- *)

let parse_complete () = Buf.message '1' ""
let bind_complete () = Buf.message '2' ""
let close_complete () = Buf.message '3' ""
let no_data () = Buf.message 'n' ""

(* one type OID per parameter (0 = unknown/unspecified) *)
let parameter_description oids =
  let b = Buffer.create 16 in
  Buf.add_int16 b (List.length oids);
  List.iter (fun o -> Buf.add_int32 b o) oids;
  Buf.message 't' (Buffer.contents b)

(* --- frontend (client -> server) messages --- *)

(* signed int32 (message payloads use -1 for NULL / absent) *)
let get_int32_signed s p =
  let v = Buf.get_int32 s p in
  if v >= 0x80000000 then v - 0x100000000 else v

let max_msg_len = 0x10000000 (* 256 MB sanity cap *)

(* read a message length as SIGNED and reject bogus values, so a corrupt/hostile
   length (e.g. 0xFFFFFFFF) can't trigger a multi-GB allocation *)
let read_len ic =
  let s = really_input_string ic 4 in
  let len = get_int32_signed s 0 in
  if len < 4 || len > max_msg_len then failwith (Printf.sprintf "invalid message length %d" len);
  len

(* Startup / SSLRequest / GSSRequest carry no tag: int32 length then payload.
   Returns the raw payload (its first int32 is the version or request code). *)
let read_startup ic =
  let len = read_len ic in
  really_input_string ic (len - 4)

(* Regular tagged message: tag + int32 length + payload. Raises End_of_file
   when the client disconnects. Returns (tag, payload). *)
let read_message ic =
  let tag = input_char ic in
  let len = read_len ic in
  let payload = if len > 4 then really_input_string ic (len - 4) else "" in
  (tag, payload)
