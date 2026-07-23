(* page.ml — a slotted page, the unit of on-disk storage in real databases
   (PostgreSQL uses exactly this shape).

   Fixed 8 KB block. Layout:
       [ num_slots:int16 ][ free_end:int16 ][ slot0 ][ slot1 ]...  ->  growing right
       ...................................................[ tupleN ]...[ tuple0 ]  <- growing left
   Each slot is (offset:int16, len:int16) pointing at a tuple. Slot array grows
   forward from the header; tuple data grows backward from the end of the page.
   Free space is the gap between the two. This lets tuples be variable length and
   move without disturbing the slot numbers row ids reference. *)

let page_size = 8192
let header = 4 (* num_slots(2) + free_end(2) *)
let slot_sz = 4 (* offset(2) + len(2) *)

type t = bytes (* always exactly page_size bytes *)

(* in-place big-endian int16 get/set over bytes *)
let g16 p o = (Char.code (Bytes.get p o) lsl 8) lor Char.code (Bytes.get p (o + 1))

let s16 p o v =
  Bytes.set p o (Char.chr ((v lsr 8) land 0xff));
  Bytes.set p (o + 1) (Char.chr (v land 0xff))

let create () =
  let p = Bytes.make page_size '\000' in
  s16 p 0 0;
  (* num_slots *)
  s16 p 2 page_size;
  (* free_end points at end of page *)
  p

let to_bytes (p : t) : bytes = p

(* accept a possibly-short buffer (e.g. truncated file) by padding to page_size *)
let of_bytes b : t =
  if Bytes.length b = page_size then b
  else begin
    let p = Bytes.make page_size '\000' in
    Bytes.blit b 0 p 0 (min (Bytes.length b) page_size);
    p
  end

(* add a tuple; returns false if the page is full *)
let add p tup =
  let ns = g16 p 0 and fe = g16 p 2 in
  let len = Bytes.length tup in
  let slot_area_end = header + (ns * slot_sz) in
  let free = fe - slot_area_end in
  if slot_sz + len > free then false
  else begin
    let off = fe - len in
    Bytes.blit tup 0 p off len;
    let so = slot_area_end in
    s16 p so off;
    s16 p (so + 2) len;
    s16 p 0 (ns + 1);
    s16 p 2 off;
    true
  end

(* all tuples, in slot order *)
let tuples p =
  let ns = g16 p 0 in
  let rec go i acc =
    if i < 0 then acc
    else
      let so = header + (i * slot_sz) in
      let off = g16 p so and len = g16 p (so + 2) in
      go (i - 1) (Bytes.sub p off len :: acc)
  in
  go (ns - 1) []

(* self-check: run via `main test` *)
let demo () =
  let p = create () in
  assert (add p (Bytes.of_string "hello"));
  assert (add p (Bytes.of_string "wo"));
  (match List.map Bytes.to_string (tuples p) with
   | [ "hello"; "wo" ] -> ()
   | _ -> assert false);
  (* survives a round-trip through raw bytes *)
  let p2 = of_bytes (to_bytes p) in
  assert (List.map Bytes.to_string (tuples p2) = [ "hello"; "wo" ]);
  print_endline "page: ok"
