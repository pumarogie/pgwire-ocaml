(* catalog.ml — the storage engine.

   In-memory tables are the working copy; every table is also persisted to disk
   as a slotted heap page (rows) plus a one-line schema file (columns). On
   startup [load] rebuilds the in-memory tables from disk. Data lives under
   $PGWIRE_DATA (default ./pgdata).

   Tables may carry ordered secondary indexes (balanced tree, stdlib Map) that
   support both equality and range lookups — the access path a B-tree provides.

   ponytail: one page per table (~8 KB cap); whole-page rewrite on each insert;
   indexes are in-memory (re-CREATE after restart). Real engines add overflow
   pages, incremental writes, and on-disk B-trees. *)

type typ = Int | Text | Bool | Float

type value = VInt of int | VText of string | VBool of bool | VFloat of float | VNull

type cmp = Eq | Lt | Le | Gt | Ge

(* total order over values. Same-type compares directly; int/float compare
   numerically; otherwise a fixed rank keeps the order total (needed for Map). *)
let rank = function VNull -> 0 | VInt _ -> 1 | VFloat _ -> 2 | VBool _ -> 3 | VText _ -> 4

let compare_value a b =
  match (a, b) with
  | VInt x, VInt y -> compare x y
  | VFloat x, VFloat y -> compare x y
  | VInt x, VFloat y -> compare (float_of_int x) y
  | VFloat x, VInt y -> compare x (float_of_int y)
  | VBool x, VBool y -> compare x y
  | VText x, VText y -> compare x y
  | VNull, VNull -> 0
  | _ -> compare (rank a) (rank b)

module VMap = Map.Make (struct
  type t = value

  let compare = compare_value
end)

(* a row is a value array — O(1) column access via row.(i) *)
(* an ordered index: key value -> the rows carrying it (duplicates kept) *)
type index = { icol : string; mutable tree : value array list VMap.t }

type table = {
  cols : (string * typ) list;
  mutable rev : value array list; (* rows newest-first; prepend is O(1) *)
  mutable indexes : index list;
  mutable last : Page.t; (* last page after the most recent checkpoint *)
  mutable npages : int;
  mutable wal : out_channel option; (* append-only write-ahead log channel *)
  mutable fwd_cache : value array list option; (* memoized insertion-order rows *)
}

(* rows in insertion order. The forward list is memoized and reused across
   queries; a write (insert/set_rows/restore) invalidates it. *)
let rows t =
  match t.fwd_cache with
  | Some r -> r
  | None ->
    let r = List.rev t.rev in
    t.fwd_cache <- Some r;
    r

let tables : (string, table) Hashtbl.t = Hashtbl.create 16

let find name = Hashtbl.find_opt tables name

let col_index t name =
  let rec go i = function
    | [] -> None
    | (c, _) :: _ when c = name -> Some i
    | _ :: tl -> go (i + 1) tl
  in
  go 0 t.cols

(* --- on-disk locations --- *)

(* read at call time so tests (and restarts) can point PGWIRE_DATA elsewhere *)
let data_dir () = try Sys.getenv "PGWIRE_DATA" with Not_found -> "pgdata"
let path name ext = Filename.concat (data_dir ()) (name ^ ext)
let ensure_dir () =
  let d = data_dir () in
  if not (Sys.file_exists d) then Unix.mkdir d 0o755

let typ_to_string = function Int -> "int" | Text -> "text" | Bool -> "bool" | Float -> "float"
let typ_of_string = function
  | "int" -> Int
  | "text" -> Text
  | "bool" -> Bool
  | "float" -> Float
  | s -> failwith (Printf.sprintf "unknown column type %s" s)

(* --- tuple serialization (self-describing: a tag byte per value) --- *)

let encode_row row =
  let b = Buffer.create 64 in
  Array.iter
    (function
      | VNull -> Buffer.add_char b '\000'
      | VInt n -> Buffer.add_char b '\001'; Buf.add_int32 b n
      | VText s -> Buffer.add_char b '\002'; Buf.add_int16 b (String.length s); Buffer.add_string b s
      | VBool v -> Buffer.add_char b '\003'; Buffer.add_char b (if v then '\001' else '\000')
      | VFloat f ->
        (* store as an exact-round-trip decimal string *)
        let s = Printf.sprintf "%.17g" f in
        Buffer.add_char b '\004'; Buf.add_int16 b (String.length s); Buffer.add_string b s)
    row;
  Buffer.to_bytes b

let decode_row bytes =
  let s = Bytes.to_string bytes in
  let n = String.length s in
  let p = ref 0 in
  let acc = ref [] in
  while !p < n do
    (match s.[!p] with
     | '\000' -> incr p; acc := VNull :: !acc
     | '\001' ->
       let v = Buf.get_int32 s (!p + 1) in
       let v = if v >= 0x80000000 then v - 0x100000000 else v in
       p := !p + 5;
       acc := VInt v :: !acc
     | '\002' ->
       let len = Buf.get_int16 s (!p + 1) in
       acc := VText (String.sub s (!p + 3) len) :: !acc;
       p := !p + 3 + len
     | '\003' -> acc := VBool (s.[!p + 1] = '\001') :: !acc; p := !p + 2
     | '\004' ->
       let len = Buf.get_int16 s (!p + 1) in
       acc := VFloat (float_of_string (String.sub s (!p + 3) len)) :: !acc;
       p := !p + 3 + len
     | c -> failwith (Printf.sprintf "corrupt tuple tag %d" (Char.code c)))
  done;
  Array.of_list (List.rev !acc)

(* --- persistence --- *)

let write_schema name cols =
  let oc = open_out (path name ".schema") in
  output_string oc (String.concat "," (List.map (fun (c, t) -> c ^ ":" ^ typ_to_string t) cols));
  close_out oc

(* write a single 8 KB page at its slot (used to init the empty page on create) *)
let write_page_at name idx pg =
  let oc = open_out_gen [ Open_wronly; Open_creat ] 0o644 (path name ".page") in
  seek_out oc (idx * Page.page_size);
  output_bytes oc (Page.to_bytes pg);
  close_out oc

(* --- write-ahead log: INSERT appends the tuple here (sequential, ~16 bytes)
   instead of rewriting an 8 KB page. Folded into pages at checkpoint. --- *)
let wal_channel name t =
  match t.wal with
  | Some oc -> oc
  | None ->
    let oc = open_out_gen [ Open_append; Open_creat ] 0o644 (path name ".wal") in
    t.wal <- Some oc;
    oc

let wal_append oc tup =
  let b = Buffer.create 8 in
  Buf.add_int32 b (Bytes.length tup);
  output_string oc (Buffer.contents b);
  output_bytes oc tup;
  flush oc

(* replay the WAL: each entry is int32 length + tuple bytes *)
let read_wal name =
  let f = path name ".wal" in
  if not (Sys.file_exists f) then []
  else begin
    let ic = open_in_bin f in
    let rows = ref [] in
    (try
       while true do
         let len = Buf.get_int32 (really_input_string ic 4) 0 in
         rows := decode_row (Bytes.of_string (really_input_string ic len)) :: !rows
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !rows
  end

(* force the WAL to disk (durability boundary — call on COMMIT / autocommit write).
   ponytail: plain fsync; true power-loss durability on macOS needs F_FULLFSYNC. *)
let sync_wal t =
  match t.wal with
  | Some oc -> ( try flush oc; Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ())
  | None -> ()

let fsync_all () = Hashtbl.iter (fun _ t -> sync_wal t) tables

let wal_clear name t =
  (match t.wal with Some oc -> ( try close_out oc with _ -> ()) | None -> ());
  t.wal <- None;
  try Sys.remove (path name ".wal") with _ -> ()

(* repack ALL rows into pages and rewrite the whole heap file — used on
   create/DELETE/UPDATE/rollback, not on plain INSERT. Resets [last]/[npages]. *)
let rewrite_pages name t =
  let pages = ref [] and cur = ref (Page.create ()) in
  List.iter
    (fun row ->
      let tup = encode_row row in
      if not (Page.add !cur tup) then begin
        pages := !cur :: !pages;
        cur := Page.create ();
        if not (Page.add !cur tup) then failwith "row too large for a single page"
      end)
    (rows t);
  pages := !cur :: !pages;
  let ordered = List.rev !pages in
  let oc = open_out_bin (path name ".page") in
  List.iter (fun p -> output_bytes oc (Page.to_bytes p)) ordered;
  close_out oc;
  t.last <- !cur;
  t.npages <- List.length ordered;
  wal_clear name t (* rows are now durable in pages; discard the WAL *)

let create name cols =
  if Hashtbl.mem tables name then
    failwith (Printf.sprintf "relation \"%s\" already exists" name);
  let t = { cols; rev = []; indexes = []; last = Page.create (); npages = 1; wal = None; fwd_cache = None } in
  Hashtbl.replace tables name t;
  ensure_dir ();
  write_schema name cols;
  write_page_at name 0 t.last

(* --- ordered secondary indexes --- *)

(* type-tagged string key for GROUP BY bucketing (not the index) *)
let index_key = function
  | VInt n -> "i" ^ string_of_int n
  | VText s -> "t" ^ s
  | VBool b -> if b then "b1" else "b0"
  | VFloat f -> "f" ^ Printf.sprintf "%.17g" f
  | VNull -> "n"

let find_index t col = List.find_opt (fun ix -> ix.icol = col) t.indexes

let tree_add tree k row =
  VMap.update k (function None -> Some [ row ] | Some rs -> Some (rs @ [ row ])) tree

let create_index name col =
  match find name with
  | None -> failwith (Printf.sprintf "relation \"%s\" does not exist" name)
  | Some t ->
    let ci =
      match col_index t col with
      | Some i -> i
      | None -> failwith (Printf.sprintf "column \"%s\" does not exist" col)
    in
    let tree = List.fold_left (fun tr row -> tree_add tr row.(ci) row) VMap.empty (rows t) in
    t.indexes <- { icol = col; tree } :: List.filter (fun ix -> ix.icol <> col) t.indexes

let lookup_index t col v =
  match find_index t col with
  | None -> []
  | Some ix -> ( match VMap.find_opt v ix.tree with Some rs -> rs | None -> [])

(* range lookup over the ordered index. Walks only the matching key range via
   the balanced tree's ordered Seq (O(log n + k)), not the whole map. NULL keys
   never satisfy a comparison (three-valued logic) so they are excluded, matching
   the seq-scan path. Accumulated with tail-recursive rev_append so a large range
   (many k) can't overflow the connection thread's stack. *)
let lookup_range t col op v =
  match find_index t col with
  | None -> []
  | Some ix ->
    let m = ix.tree in
    let acc = ref [] in (* matching rows, reverse order *)
    let add rs = acc := List.rev_append rs !acc in
    (match op with
     | Eq -> ( match VMap.find_opt v m with Some rs -> add rs | None -> ())
     | Gt | Ge ->
       (* keys >= v ascending; for Gt skip the == v group. NULLs sort below v so
          to_seq_from never yields them. *)
       Seq.iter (fun (k, rs) -> if op = Ge || compare_value k v > 0 then add rs) (VMap.to_seq_from v m)
     | Lt | Le ->
       (* ascending from the start; skip NULLs, stop once past the bound *)
       let exception Done in
       (try
          Seq.iter
            (fun (k, rs) ->
              if k = VNull then ()
              else if (op = Lt && compare_value k v < 0) || (op = Le && compare_value k v <= 0) then add rs
              else raise Done)
            (VMap.to_seq m)
        with Done -> ()));
    List.rev !acc

(* crude selectivity for a numeric range predicate: interpolate v within the
   index's [min,max] (O(log n) to fetch bounds) — the same idea PG's planner uses
   from pg_stats. Returns the estimated fraction of rows the predicate matches,
   or None if not estimable (non-numeric, empty, or NULL-min index).
   ponytail: linear interpolation, assumes uniform distribution; a histogram
   would handle skew. *)
let range_fraction t col op v =
  match (find_index t col, v) with
  | Some ix, (VInt _ | VFloat _) when not (VMap.is_empty ix.tree) ->
    let num = function VInt n -> float_of_int n | VFloat f -> f | _ -> nan in
    let lo = num (fst (VMap.min_binding ix.tree)) and hi = num (fst (VMap.max_binding ix.tree)) in
    let x = num v in
    if hi <= lo || lo <> lo || hi <> hi then None
    else
      let clamp f = if f < 0. then 0. else if f > 1. then 1. else f in
      Some
        (clamp
           (match op with Gt | Ge -> (hi -. x) /. (hi -. lo) | Lt | Le -> (x -. lo) /. (hi -. lo) | Eq -> 0.))
  | _ -> None

(* O(1) amortized: prepend the row, update indexes, append the tuple to the last
   page and rewrite only that page (not the whole heap). *)
(* rows in a column's index order — lets ORDER BY on an indexed column skip the
   sort (walks the balanced tree in key order). With [limit] it stops early, so
   ORDER BY ... LIMIT n becomes a top-k that touches only n rows, not the table.
   None if the column has no index. Ascending by compare_value (NULLs first). *)
let scan_ordered t col ~desc ?limit () =
  match find_index t col with
  | None -> None
  | Some ix ->
    let seq = if desc then VMap.to_rev_seq ix.tree else VMap.to_seq ix.tree in
    let cap = match limit with Some n -> n | None -> max_int in
    let out = ref [] and count = ref 0 in
    (try
       Seq.iter
         (fun (_, rs) ->
           List.iter
             (fun row -> if !count >= cap then raise Exit else (out := row :: !out; incr count))
             rs)
         seq
     with Exit -> ());
    Some (List.rev !out)

(* O(1): prepend the row, update indexes, append the tuple to the WAL (a small
   sequential write — no 8 KB page rewrite). Pages are updated at checkpoint. *)
let insert name t row =
  t.rev <- row :: t.rev;
  t.fwd_cache <- None;
  List.iter
    (fun ix ->
      match col_index t ix.icol with
      | Some i -> ix.tree <- tree_add ix.tree row.(i) row
      | None -> ())
    t.indexes;
  wal_append (wal_channel name t) (encode_row row)

(* fold the WAL into pages and clear it — makes recent inserts page-resident *)
let checkpoint name = match find name with Some t -> rewrite_pages name t | None -> ()

(* rebuild every index tree from the current rows (after a bulk mutation) *)
let reindex t =
  List.iter
    (fun ix ->
      match col_index t ix.icol with
      | Some ci -> ix.tree <- List.fold_left (fun tr row -> tree_add tr row.(ci) row) VMap.empty (rows t)
      | None -> ())
    t.indexes

(* replace a table's rows wholesale (DELETE/UPDATE): fix indexes + rewrite heap *)
let set_rows name t rows_fwd =
  t.rev <- List.rev rows_fwd;
  t.fwd_cache <- None;
  reindex t;
  rewrite_pages name t

(* --- transactions: whole-catalog snapshot / restore --- *)

(* Row lists are immutable and replaced wholesale on mutation, so capturing the
   current list reference per table is a cheap, safe snapshot.
   ponytail: snapshots the whole catalog; no isolation between concurrent
   transactions (real engines use MVCC + a WAL). *)
(* capture rows AND the set of indexed columns per table, so rollback also
   reverts indexes created inside the transaction *)
type snapshot = (string, value array list * string list) Hashtbl.t

let snapshot () : snapshot =
  let s = Hashtbl.create 16 in
  Hashtbl.iter (fun name t -> Hashtbl.replace s name (t.rev, List.map (fun ix -> ix.icol) t.indexes)) tables;
  s

let restore (s : snapshot) =
  (* drop tables that did not exist when the snapshot was taken *)
  let created = Hashtbl.fold (fun name _ acc -> if Hashtbl.mem s name then acc else name :: acc) tables [] in
  List.iter
    (fun name ->
      Hashtbl.remove tables name;
      (try Sys.remove (path name ".schema") with _ -> ());
      try Sys.remove (path name ".page") with _ -> ())
    created;
  (* restore rows, rebuild exactly the snapshotted indexes, rewrite pages *)
  Hashtbl.iter
    (fun name (rev, idx_cols) ->
      match find name with
      | None -> ()
      | Some t ->
        t.rev <- rev;
        t.fwd_cache <- None;
        t.indexes <-
          List.filter_map
            (fun col ->
              match col_index t col with
              | Some ci -> Some { icol = col; tree = List.fold_left (fun tr row -> tree_add tr row.(ci) row) VMap.empty (rows t) }
              | None -> None)
            idx_cols;
        rewrite_pages name t)
    s

(* rebuild in-memory tables from disk at startup *)
let load () =
  if Sys.file_exists (data_dir ()) then
    Array.iter
      (fun f ->
        if Filename.check_suffix f ".schema" then begin
          let name = Filename.chop_suffix f ".schema" in
          let ic = open_in (path name ".schema") in
          let line = try input_line ic with End_of_file -> "" in
          close_in ic;
          let cols =
            if line = "" then []
            else
              List.map
                (fun seg ->
                  match String.split_on_char ':' seg with
                  | [ c; t ] -> (c, typ_of_string t)
                  | _ -> failwith "corrupt schema file")
                (String.split_on_char ',' line)
          in
          let pf = path name ".page" in
          let chunks =
            if Sys.file_exists pf then begin
              let ic = open_in_bin pf in
              let raw = really_input_string ic (in_channel_length ic) in
              close_in ic;
              (* split the heap file into 8 KB page-sized chunks *)
              let n = String.length raw in
              let rec go off acc =
                if off >= n then List.rev acc
                else
                  let sz = min Page.page_size (n - off) in
                  go (off + sz) (String.sub raw off sz :: acc)
              in
              go 0 []
            end
            else []
          in
          (* tail-recursive decode so a multi-million-row table can't overflow *)
          let page_rows =
            List.rev
              (List.fold_left
                 (fun acc ch ->
                   List.fold_left (fun a tup -> decode_row tup :: a) acc (Page.tuples (Page.of_bytes (Bytes.of_string ch))))
                 [] chunks)
          in
          (* pages are the last checkpoint; replay the WAL on top *)
          let fwd = page_rows @ read_wal name in
          let last = match List.rev chunks with c :: _ -> Page.of_bytes (Bytes.of_string c) | [] -> Page.create () in
          let npages = match chunks with [] -> 1 | _ -> List.length chunks in
          Hashtbl.replace tables name { cols; rev = List.rev fwd; indexes = []; last; npages; wal = None; fwd_cache = None }
        end)
      (Sys.readdir (data_dir ()))
