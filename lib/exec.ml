(* exec.ml — run a parsed statement against the catalog.

   Handles: the query planner (index vs seq scan), projection, aggregates
   (COUNT/SUM/MIN/MAX/AVG) with GROUP BY, ORDER BY, LIMIT, and $n bind
   parameters. Errors are raised as [Sql_error (sqlstate, message)]. *)

exception Sql_error of string * string

let col_desc name typ =
  match typ with
  | Catalog.Int -> (name, 23, 4) (* int4 *)
  | Catalog.Text -> (name, 25, -1) (* text, variable length *)
  | Catalog.Bool -> (name, 16, 1) (* bool *)
  | Catalog.Float -> (name, 701, 8) (* float8 *)

type result =
  | Rows of (string * int * int) list * string option list list
  | Tag of string

let text_of_value = function
  | Catalog.VInt n -> Some (string_of_int n)
  | Catalog.VText s -> Some s
  | Catalog.VBool b -> Some (if b then "t" else "f") (* PostgreSQL bool text format *)
  | Catalog.VFloat f -> Some (Printf.sprintf "%g" f)
  | Catalog.VNull -> None

(* evaluate a WHERE comparison against the ordered value comparison *)
let cmp_ok op a b =
  let c = Catalog.compare_value a b in
  match op with
  | Catalog.Eq -> c = 0
  | Catalog.Lt -> c < 0
  | Catalog.Le -> c <= 0
  | Catalog.Gt -> c > 0
  | Catalog.Ge -> c >= 0

let col_typ t c =
  match List.assoc_opt c t.Catalog.cols with
  | Some ty -> ty
  | None -> raise (Sql_error ("42703", Printf.sprintf "column \"%s\" does not exist" c))

let col_idx t c =
  match Catalog.col_index t c with
  | Some i -> i
  | None -> raise (Sql_error ("42703", Printf.sprintf "column \"%s\" does not exist" c))

(* coerce a value to a column's declared type at the write boundary, so a
   literal or an inferred bind param of the wrong type is stored as the column's
   type (e.g. numeric literal into a text column, text param into an int column). *)
let bad_input s = raise (Sql_error ("22P02", Printf.sprintf "invalid input syntax: \"%s\"" s))

let coerce typ v =
  match (typ, v) with
  | _, Catalog.VNull -> Catalog.VNull
  | Catalog.Int, Catalog.VInt _ -> v
  | Catalog.Int, Catalog.VFloat f -> Catalog.VInt (int_of_float f)
  | Catalog.Int, Catalog.VText s -> ( match int_of_string_opt s with Some n -> Catalog.VInt n | None -> bad_input s)
  | Catalog.Float, Catalog.VFloat _ -> v
  | Catalog.Float, Catalog.VInt n -> Catalog.VFloat (float_of_int n)
  | Catalog.Float, Catalog.VText s -> ( match float_of_string_opt s with Some f -> Catalog.VFloat f | None -> bad_input s)
  | Catalog.Text, Catalog.VText _ -> v
  | Catalog.Text, Catalog.VInt n -> Catalog.VText (string_of_int n)
  | Catalog.Text, Catalog.VFloat f -> Catalog.VText (Printf.sprintf "%g" f)
  | Catalog.Text, Catalog.VBool b -> Catalog.VText (if b then "t" else "f")
  | Catalog.Bool, Catalog.VBool _ -> v
  | Catalog.Bool, Catalog.VText ("t" | "true") -> Catalog.VBool true
  | Catalog.Bool, Catalog.VText ("f" | "false") -> Catalog.VBool false
  | _ -> raise (Sql_error ("42804", "column type mismatch"))

let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: tl -> x :: take (n - 1) tl
let rec tdrop n l = if n <= 0 then l else match l with [] -> [] | _ :: tl -> tdrop (n - 1) tl

(* resolve a LIMIT/OFFSET pvalue (params already bound) to a non-negative int *)
let pv_int what = function
  | None -> None
  | Some (Sql.Lit (Catalog.VInt n)) when n >= 0 -> Some n
  | Some _ -> raise (Sql_error ("2201W", Printf.sprintf "%s must be a non-negative integer" what))

(* apply OFFSET then LIMIT to a row list *)
let slice sel rows =
  let rows = match pv_int "OFFSET" sel.Sql.offset with None -> rows | Some n -> tdrop n rows in
  match pv_int "LIMIT" sel.Sql.limit with None -> rows | Some n -> take n rows

(* Tail-recursive, order-preserving list ops. Used on ROW-scale lists so a large
   result set can't overflow the connection thread's small stack — the stdlib
   List.map/filter/concat_map/@ are non-tail-recursive in OCaml 4.12. *)
let tmap f l = List.rev (List.rev_map f l)
let tfilter p l = List.rev (List.fold_left (fun a x -> if p x then x :: a else a) [] l)
let tfilter_map f l = List.rev (List.fold_left (fun a x -> match f x with Some y -> y :: a | None -> a) [] l)
let tconcat_map f l = List.rev (List.fold_left (fun a x -> List.rev_append (f x) a) [] l)
let tappend a b = List.rev_append (List.rev a) b

(* drop duplicate output rows, preserving first-seen order (SELECT DISTINCT) *)
let dedupe rows =
  let seen = Hashtbl.create 64 in
  tfilter (fun r -> if Hashtbl.mem seen r then false else (Hashtbl.add seen r (); true)) rows

(* --- bind parameters (extended query protocol) --- *)

let deref = function
  | Sql.Lit v -> v
  | Sql.Param n -> raise (Sql_error ("42P02", Printf.sprintf "bind parameter $%d not supplied" n))

let bind_pvalue params = function
  | Sql.Lit v -> Sql.Lit v
  | Sql.Param n ->
    if n >= 1 && n <= Array.length params then Sql.Lit params.(n - 1)
    else raise (Sql_error ("42P02", Printf.sprintf "bind parameter $%d not supplied" n))

let rec bind_pred params = function
  | Sql.Cmp (c, op, pv) -> Sql.Cmp (c, op, bind_pvalue params pv)
  | Sql.Null _ as p -> p
  | Sql.And (a, b) -> Sql.And (bind_pred params a, bind_pred params b)
  | Sql.Or (a, b) -> Sql.Or (bind_pred params a, bind_pred params b)

let bind_cond params = function None -> None | Some p -> Some (bind_pred params p)

let bind params stmt =
  match stmt with
  | Sql.Insert (t, vs) -> Sql.Insert (t, List.map (bind_pvalue params) vs)
  | Sql.Select s ->
    Sql.Select
      { s with
        Sql.where = bind_cond params s.Sql.where;
        Sql.limit = Option.map (bind_pvalue params) s.Sql.limit;
        Sql.offset = Option.map (bind_pvalue params) s.Sql.offset }
  | Sql.Delete (t, w) -> Sql.Delete (t, bind_cond params w)
  | Sql.Update (t, a, w) ->
    Sql.Update (t, List.map (fun (c, pv) -> (c, bind_pvalue params pv)) a, bind_cond params w)
  | other -> other

let count_params stmt =
  let m = ref 0 in
  let scan = function Sql.Param n -> if n > !m then m := n | Sql.Lit _ -> () in
  let rec scan_pred = function
    | Sql.Cmp (_, _, pv) -> scan pv
    | Sql.Null _ -> ()
    | Sql.And (a, b) | Sql.Or (a, b) -> scan_pred a; scan_pred b
  in
  let scan_cond = function Some p -> scan_pred p | None -> () in
  (match stmt with
   | Sql.Insert (_, vs) -> List.iter scan vs
   | Sql.Select s ->
     scan_cond s.Sql.where;
     Option.iter scan s.Sql.limit;
     Option.iter scan s.Sql.offset
   | Sql.Delete (_, w) -> scan_cond w
   | Sql.Update (_, a, w) -> List.iter (fun (_, pv) -> scan pv) a; scan_cond w
   | _ -> ());
  !m

(* evaluate a predicate given a way to look up a column's value in the current
   row ([getval]). Three-valued logic: a comparison with a NULL operand is
   UNKNOWN (treated as false); only IS [NOT] NULL tests NULLs. AND/OR combine.
   [getval] is what differs between a single table (col_index) and a join
   (combined-schema resolve). *)
let rec eval_pred getval = function
  | Sql.Cmp (c, op, pv) ->
    let v = getval c and w = deref pv in
    v <> Catalog.VNull && w <> Catalog.VNull && cmp_ok op v w
  | Sql.Null (c, neg) -> (getval c = Catalog.VNull) <> neg
  | Sql.And (a, b) -> eval_pred getval a && eval_pred getval b
  | Sql.Or (a, b) -> eval_pred getval a || eval_pred getval b

(* compile a single-table predicate ONCE into a fast [row -> bool]: column names
   are resolved to indices and the literal is dereferenced at compile time, not
   per row. NULL comparisons are folded to a constant false. *)
let rec compile_pred t = function
  | Sql.Cmp (c, op, pv) ->
    let i = col_idx t c and w = deref pv in
    if w = Catalog.VNull then fun _ -> false
    else fun row -> let v = row.(i) in v <> Catalog.VNull && cmp_ok op v w
  | Sql.Null (c, neg) ->
    let i = col_idx t c in
    fun row -> (row.(i) = Catalog.VNull) <> neg
  | Sql.And (a, b) ->
    let fa = compile_pred t a and fb = compile_pred t b in
    fun row -> fa row && fb row
  | Sql.Or (a, b) ->
    let fa = compile_pred t a and fb = compile_pred t b in
    fun row -> fa row || fb row

let compile_cond t = function None -> (fun _ -> true) | Some p -> compile_pred t p

(* --- SELECT output shape --- *)

let is_agg_query sel =
  sel.Sql.group_by <> None || sel.Sql.having <> None
  || List.exists (function Sql.Agg _ -> true | _ -> false) sel.Sql.items

let agg_name = function
  | Sql.Count -> "count"
  | Sql.Sum -> "sum"
  | Sql.Min -> "min"
  | Sql.Max -> "max"
  | Sql.Avg -> "avg"

(* plain (non-aggregate) projection -> (name, typ, row index) *)
let plain_cols t items =
  List.concat_map
    (function
      | Sql.Star -> List.mapi (fun i (c, ty) -> (c, ty, i)) t.Catalog.cols
      | Sql.Col c -> [ (c, col_typ t c, col_idx t c) ]
      | Sql.Agg _ -> raise (Sql_error ("42601", "aggregate requires GROUP BY or an aggregate-only select")))
    items

let agg_desc t items =
  List.map
    (function
      | Sql.Star -> raise (Sql_error ("42601", "cannot use * with aggregates"))
      | Sql.Col c -> col_desc c (col_typ t c)
      | Sql.Agg (Sql.Count, _) -> col_desc "count" Catalog.Int
      | Sql.Agg (Sql.Avg, _) -> col_desc "avg" Catalog.Float
      | Sql.Agg (a, Some c) -> col_desc (agg_name a) (col_typ t c) (* SUM/MIN/MAX keep the column type *)
      | Sql.Agg (a, None) -> col_desc (agg_name a) Catalog.Int)
    items

let select_output_desc t sel =
  if is_agg_query sel then agg_desc t sel.Sql.items
  else List.map (fun (c, ty, _) -> col_desc c ty) (plain_cols t sel.Sql.items)

(* --- qualified column references + JOIN --- *)

let split_ref s =
  match String.split_on_char '.' s with
  | [ c ] -> (None, c)
  | [ q; c ] -> (Some q, c)
  | _ -> raise (Sql_error ("42601", Printf.sprintf "invalid column reference %s" s))

(* strip a "tbl." qualifier for single-table queries (or reject a foreign one) *)
let strip_qual tname s =
  match split_ref s with
  | None, c -> c
  | Some q, c ->
    if q = tname then c
    else raise (Sql_error ("42P01", Printf.sprintf "invalid reference to table \"%s\"" q))

let rec strip_pred sc = function
  | Sql.Cmp (c, op, pv) -> Sql.Cmp (sc c, op, pv)
  | Sql.Null (c, neg) -> Sql.Null (sc c, neg)
  | Sql.And (a, b) -> Sql.And (strip_pred sc a, strip_pred sc b)
  | Sql.Or (a, b) -> Sql.Or (strip_pred sc a, strip_pred sc b)

let normalize_single tname sel =
  let sc = strip_qual tname in
  {
    sel with
    Sql.items =
      List.map
        (function
          | Sql.Col c -> Sql.Col (sc c)
          | Sql.Agg (a, Some c) -> Sql.Agg (a, Some (sc c))
          | x -> x)
        sel.Sql.items;
    Sql.where = (match sel.Sql.where with Some p -> Some (strip_pred sc p) | None -> None);
    Sql.order_by = (match sel.Sql.order_by with Some o -> Some { o with Sql.by = sc o.Sql.by } | None -> None);
    Sql.group_by = (match sel.Sql.group_by with Some g -> Some (sc g) | None -> None);
  }

(* inner equi-join, nested-loop.
   ponytail: O(n*m) nested loop; a hash join keyed on the join column is the
   real upgrade for large inputs. *)
let run_join ~notice name1 name2 (lref, rref) kind sel =
  let find n =
    match Catalog.find n with
    | Some t -> t
    | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" n))
  in
  let t1 = find name1 and t2 = find name2 in
  let schema =
    List.map (fun (c, ty) -> (name1, c, ty)) t1.Catalog.cols
    @ List.map (fun (c, ty) -> (name2, c, ty)) t2.Catalog.cols
  in
  let indexed = List.mapi (fun i (q, c, _) -> (i, q, c)) schema in
  (* resolve a (possibly-qualified) ref to an index in the combined row *)
  let resolve s =
    let q, c = split_ref s in
    match List.filter (fun (_, q', c') -> c' = c && (match q with None -> true | Some x -> x = q')) indexed with
    | [ (i, _, _) ] -> i
    | [] -> raise (Sql_error ("42703", Printf.sprintf "column \"%s\" does not exist" s))
    | _ -> raise (Sql_error ("42702", Printf.sprintf "column reference \"%s\" is ambiguous" s))
  in
  (* which table + local index a join-condition ref points at *)
  let side s =
    let q, c = split_ref s in
    match q with
    | Some x when x = name1 -> (`L, col_idx t1 c)
    | Some x when x = name2 -> (`R, col_idx t2 c)
    | Some x -> raise (Sql_error ("42P01", Printf.sprintf "unknown table \"%s\"" x))
    | None -> (
      match Catalog.col_index t1 c with
      | Some i -> (`L, i)
      | None -> ( match Catalog.col_index t2 c with Some i -> (`R, i) | None -> raise (Sql_error ("42703", Printf.sprintf "column \"%s\" does not exist" s))))
  in
  let ls, li = side lref and rs, ri = side rref in
  let pick s i r1 r2 = (match s with `L -> r1 | `R -> r2).(i) in
  let null_left = Array.make (List.length t1.Catalog.cols) Catalog.VNull in
  let null_right = Array.make (List.length t2.Catalog.cols) Catalog.VNull in
  let keep_left = kind = Sql.Left || kind = Sql.Full in
  let keep_right = kind = Sql.Right || kind = Sql.Full in
  let r2s = Array.of_list (Catalog.rows t2) in
  let matched = Array.make (Array.length r2s) false in
  (* Join-key string: numeric (int==float compare numerically), text, bool; NULL
     yields None so NULL keys never match (SQL: NULL = NULL is false). Same key
     rule for both algorithms, so hash and nested loop agree exactly. *)
  (* normalized value key (no string alloc): ints promote to float so 5 = 5.0
     joins; NULL yields None so NULL keys never match *)
  let jkey = function
    | Catalog.VNull -> None
    | Catalog.VInt n -> Some (Catalog.VFloat (float_of_int n))
    | v -> Some v
  in
  (* the ON condition's two operands: one column in t1, one in t2 *)
  let cross = match (ls, rs) with `L, `R -> Some (li, ri) | `R, `L -> Some (ri, li) | _ -> None in
  (* for RIGHT/FULL: right rows that never matched, NULL-padded on the left *)
  let unmatched_right () =
    if not keep_right then []
    else
      Array.to_list (Array.mapi (fun j r2 -> (j, r2)) r2s)
      |> tfilter_map (fun (j, r2) -> if matched.(j) then None else Some (Array.append null_left r2))
  in
  (* nested loop: O(n*m), handles any ON shape *)
  let nested_loop_join () =
    let eq r1 r2 =
      match (jkey (pick ls li r1 r2), jkey (pick rs ri r1 r2)) with Some a, Some b -> a = b | _ -> false
    in
    (* bind left first so [matched] is populated before unmatched_right reads it *)
    let left =
      tconcat_map
        (fun r1 ->
          let ms = ref [] in
          Array.iteri (fun j r2 -> if eq r1 r2 then (matched.(j) <- true; ms := (Array.append r1 r2) :: !ms)) r2s;
          match !ms with [] when keep_left -> [ Array.append r1 null_right ] | _ -> List.rev !ms)
        (Catalog.rows t1)
    in
    tappend left (unmatched_right ())
  in
  (* hash join: O(n+m). Build a hash table on the right, probe with the left —
     exactly how PostgreSQL's hash join emits unmatched inner rows for RIGHT/FULL
     via the [matched] flags. *)
  let hash_join i1 i2 =
    let ht = Hashtbl.create ((2 * Array.length r2s) + 1) in
    Array.iteri (fun j r2 -> match jkey (r2.(i2)) with Some k -> Hashtbl.add ht k (r2, j) | None -> ()) r2s;
    let left =
      tconcat_map
        (fun r1 ->
          let ms = match jkey (r1.(i1)) with Some k -> Hashtbl.find_all ht k | None -> [] in
          List.iter (fun (_, j) -> matched.(j) <- true) ms;
          match ms with [] when keep_left -> [ Array.append r1 null_right ] | _ -> tmap (fun (r2, _) -> Array.append r1 r2) ms)
        (Catalog.rows t1)
    in
    tappend left (unmatched_right ())
  in
  (* planner: hash join for larger equi-joins, nested loop for tiny inputs.
     ponytail: fixed threshold; PostgreSQL chooses by a real cost model. *)
  let hash_threshold = 8 in
  let joined =
    match cross with
    | Some (i1, i2) when min (List.length (Catalog.rows t1)) (Array.length r2s) > hash_threshold ->
      notice (Printf.sprintf "hash join %s x %s" name1 name2);
      hash_join i1 i2
    | _ ->
      notice (Printf.sprintf "nested loop join %s x %s" name1 name2);
      nested_loop_join ()
  in
  let proj =
    List.concat_map
      (function
        | Sql.Star -> List.mapi (fun i (_, c, ty) -> (c, ty, i)) schema
        | Sql.Col s -> let i = resolve s in let _, c, ty = List.nth schema i in [ (c, ty, i) ]
        | Sql.Agg _ -> raise (Sql_error ("0A000", "aggregates are not supported with JOIN")))
      sel.Sql.items
  in
  let rows =
    match sel.Sql.where with
    | None -> joined
    | Some p -> tfilter (fun row -> eval_pred (fun c -> row.((resolve c))) p) joined
  in
  let rows =
    match sel.Sql.order_by with
    | None -> rows
    | Some o ->
      let i = resolve o.Sql.by in
      let s = List.stable_sort (fun a b -> Catalog.compare_value (a.(i)) (b.(i))) rows in
      if o.Sql.desc then List.rev s else s
  in
  let out = tmap (fun row -> List.map (fun (_, _, i) -> text_of_value (row.(i))) proj) rows in
  let out = if sel.Sql.distinct then dedupe out else out in
  let out = slice sel out in
  Rows (List.map (fun (c, ty, _) -> col_desc c ty) proj, out)

(* output column descriptions without executing (Describe message) *)
let describe stmt =
  match stmt with
  | Sql.Select sel -> (
    match sel.Sql.from with
    | Sql.Table tname -> (
      let sel = normalize_single tname sel in
      match Catalog.find tname with
      | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" tname))
      | Some t -> Some (select_output_desc t sel))
    | Sql.Join (a, b, on, k) -> (
      match run_join ~notice:(fun _ -> ()) a b on k sel with Rows (desc, _) -> Some desc | Tag _ -> None))
  | _ -> None


(* --- executing SELECT --- *)

let run_plain ?(presorted = false) t sel candidates =
  let cols = plain_cols t sel.Sql.items in
  let idxs = List.map (fun (_, _, i) -> i) cols in
  let candidates =
    if presorted then candidates (* rows already came ordered from the index *)
    else
      match sel.Sql.order_by with
      | None -> candidates
      | Some o ->
        let i = col_idx t o.Sql.by in
        let s = List.stable_sort (fun r1 r2 -> Catalog.compare_value (r1.(i)) (r2.(i))) candidates in
        if o.Sql.desc then List.rev s else s
  in
  (* DISTINCT on the selected column VALUES, before rendering, so duplicates are
     never stringified (big win when few distinct rows survive). Then LIMIT. *)
  let candidates =
    if sel.Sql.distinct then begin
      let seen = Hashtbl.create 256 in
      tfilter
        (fun row ->
          let k = List.map (fun i -> row.(i)) idxs in
          if Hashtbl.mem seen k then false else (Hashtbl.add seen k (); true))
        candidates
    end
    else candidates
  in
  let candidates = slice sel candidates in
  let desc = List.map (fun (c, ty, _) -> col_desc c ty) cols in
  let rows = tmap (fun row -> List.map (fun i -> text_of_value row.(i)) idxs) candidates in
  Rows (desc, rows)

(* one accumulator (update-with-row, finalize-to-value) for a select/HAVING item.
   Stateful — create a fresh one per group. *)
let agg_maker t = function
  | Sql.Star -> raise (Sql_error ("42601", "cannot use * with aggregates"))
  | Sql.Col c ->
    let i = col_idx t c in
    let v = ref Catalog.VNull and got = ref false in
    ((fun row -> if not !got then (v := row.(i); got := true)), fun () -> !v)
  | Sql.Agg (Sql.Count, None) ->
    let n = ref 0 in
    ((fun _ -> incr n), fun () -> Catalog.VInt !n)
  | Sql.Agg (Sql.Count, Some c) ->
    let i = col_idx t c and n = ref 0 in
    ((fun row -> if row.(i) <> Catalog.VNull then incr n), fun () -> Catalog.VInt !n)
  | Sql.Agg (((Sql.Sum | Sql.Min | Sql.Max | Sql.Avg) as a), Some c) ->
    let i = col_idx t c and ty = col_typ t c in
    if ty <> Catalog.Int && ty <> Catalog.Float then
      raise (Sql_error ("42883", Printf.sprintf "aggregate is not defined for column \"%s\"" c));
    let sum = ref 0. and mn = ref infinity and mx = ref neg_infinity and cnt = ref 0 in
    let upd row =
      match row.(i) with
      | Catalog.VInt x ->
        let f = float_of_int x in
        sum := !sum +. f;
        if f < !mn then mn := f;
        if f > !mx then mx := f;
        incr cnt
      | Catalog.VFloat f ->
        sum := !sum +. f;
        if f < !mn then mn := f;
        if f > !mx then mx := f;
        incr cnt
      | _ -> ()
    in
    let fin () =
      if !cnt = 0 then Catalog.VNull
      else
        let r = match a with Sql.Sum -> !sum | Sql.Min -> !mn | Sql.Max -> !mx | _ -> !sum /. float_of_int !cnt in
        match a with
        | Sql.Avg -> Catalog.VFloat r
        | _ -> ( match ty with Catalog.Int -> Catalog.VInt (int_of_float r) | _ -> Catalog.VFloat r)
    in
    (upd, fin)
  | Sql.Agg (_, None) -> raise (Sql_error ("42601", "aggregate function requires a column argument"))

(* fused hash aggregation: a single pass builds one accumulator set per group
   (like PostgreSQL's HashAggregate) — no intermediate per-group row lists.
   A sample row per group serves Col items and ORDER BY. *)
let run_aggregate t sel candidates =
  let items = sel.Sql.items in
  let key_idx = match sel.Sql.group_by with Some g -> Some (col_idx t g) | None -> None in
  let having_item = match sel.Sql.having with Some (it, _, _) -> Some it | None -> None in
  let tbl = Hashtbl.create 256 and order = ref [] in
  let make row =
    let item_accs = List.map (agg_maker t) items in
    let hv = match having_item with Some it -> Some (agg_maker t it) | None -> None in
    (item_accs, hv, row)
  in
  let update (item_accs, hv, _) row =
    List.iter (fun (upd, _) -> upd row) item_accs;
    match hv with Some (upd, _) -> upd row | None -> ()
  in
  (match key_idx with
   | None ->
     (* one group over everything — no hash lookup per row; always one output row *)
     let a = make [||] in
     Hashtbl.replace tbl Catalog.VNull a;
     order := [ Catalog.VNull ];
     List.iter (update a) candidates
   | Some i ->
     List.iter
       (fun row ->
         let a =
           match Hashtbl.find_opt tbl row.(i) with
           | Some a -> a
           | None -> let a = make row in Hashtbl.replace tbl row.(i) a; order := row.(i) :: !order; a
         in
         update a row)
       candidates);
  let keys = List.rev !order in
  let keys =
    match sel.Sql.having with
    | None -> keys
    | Some (_, op, pv) ->
      let w = deref pv in
      List.filter
        (fun k ->
          match Hashtbl.find tbl k with
          | _, Some (_, fin), _ -> let v = fin () in v <> Catalog.VNull && w <> Catalog.VNull && cmp_ok op v w
          | _ -> true)
        keys
  in
  let keys =
    match sel.Sql.order_by with
    | None -> keys
    | Some o ->
      let i = col_idx t o.Sql.by in
      let sample k = let _, _, s = Hashtbl.find tbl k in s in
      let ss = List.stable_sort (fun a b -> Catalog.compare_value (sample a).(i) (sample b).(i)) keys in
      if o.Sql.desc then List.rev ss else ss
  in
  let rows =
    List.map (fun k -> let item_accs, _, _ = Hashtbl.find tbl k in List.map (fun (_, fin) -> text_of_value (fin ())) item_accs) keys
  in
  let rows = if sel.Sql.distinct then dedupe rows else rows in
  let rows = slice sel rows in
  Rows (agg_desc t sel.Sql.items, rows)

(* single-table SELECT: planner picks the access path, then project/aggregate *)
let run_table ~notice tname sel =
  match Catalog.find tname with
  | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" tname))
  | Some t -> (
    (* ORDER BY on an indexed column with no WHERE and a plain select: read rows
       already ordered from the index and skip the sort entirely. *)
    let ordered =
      match (sel.Sql.where, sel.Sql.order_by) with
      | None, Some o when (not (is_agg_query sel)) && Catalog.find_index t o.Sql.by <> None ->
        (* push LIMIT down as a top-k, unless DISTINCT (which changes the count).
           With OFFSET we need offset+limit rows, then run_plain trims exactly. *)
        let limit =
          if sel.Sql.distinct then None
          else
            match pv_int "LIMIT" sel.Sql.limit with
            | None -> None
            | Some l -> Some (l + (match pv_int "OFFSET" sel.Sql.offset with Some o -> o | None -> 0))
        in
        Catalog.scan_ordered t o.Sql.by ~desc:o.Sql.desc ?limit ()
      | _ -> None
    in
    match ordered with
    | Some rows ->
      let o = match sel.Sql.order_by with Some o -> o.Sql.by | None -> "" in
      notice (Printf.sprintf "index ordered scan on %s.%s" tname o);
      run_plain ~presorted:true t sel rows
    | None ->
      let candidates =
        match sel.Sql.where with
        (* index paths only apply to comparisons with a non-NULL literal *)
        | Some (Sql.Cmp (c, Catalog.Eq, pv)) when deref pv <> Catalog.VNull && Catalog.find_index t c <> None ->
          notice (Printf.sprintf "index scan on %s.%s" tname c);
          Catalog.lookup_index t c (deref pv)
        (* cost-based: index-range only when the predicate is selective (matches
           a small fraction); a non-selective range is cheaper as a seq scan *)
        | Some (Sql.Cmp (c, op, pv))
          when op <> Catalog.Eq && deref pv <> Catalog.VNull && Catalog.find_index t c <> None
               && (match Catalog.range_fraction t c op (deref pv) with Some f -> f <= 0.33 | None -> true) ->
          notice (Printf.sprintf "index range scan on %s.%s" tname c);
          Catalog.lookup_range t c op (deref pv)
        | Some pred ->
          let col = match pred with Sql.Cmp (c, _, _) | Sql.Null (c, _) -> c | _ -> "expr" in
          notice (Printf.sprintf "seq scan on %s (filter on %s)" tname col);
          let ok = compile_pred t pred in
          tfilter ok (Catalog.rows t)
        | None ->
          notice (Printf.sprintf "seq scan on %s" tname);
          Catalog.rows t
      in
      if is_agg_query sel then run_aggregate t sel candidates else run_plain t sel candidates)

(* [notice] receives the chosen access path ("seq scan" / "index scan"), which
   the server relays to the client as a NOTICE — the query planner made visible. *)
let run ?(notice = fun _ -> ()) stmt =
  match stmt with
  | Sql.Other tag -> Tag tag
  | Sql.Create (name, cols) ->
    (try Catalog.create name cols with Failure m -> raise (Sql_error ("42P07", m)));
    Tag "CREATE TABLE"
  | Sql.CreateIndex (tbl, col) ->
    (match Catalog.find tbl with
     | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" tbl))
     | Some _ -> ( try Catalog.create_index tbl col with Failure m -> raise (Sql_error ("42703", m))));
    Tag "CREATE INDEX"
  | Sql.Insert (name, vals) -> (
    match Catalog.find name with
    | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" name))
    | Some t ->
      let vals = List.map deref vals in
      if List.length vals <> List.length t.Catalog.cols then
        raise (Sql_error ("42601", "INSERT has wrong number of values"));
      let vals = List.map2 (fun (_, ty) v -> coerce ty v) t.Catalog.cols vals in
      Catalog.insert name t (Array.of_list vals);
      Tag "INSERT 0 1")
  | Sql.Delete (name, where) -> (
    match Catalog.find name with
    | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" name))
    | Some t ->
      let before = List.length (Catalog.rows t) in
      let ok = compile_cond t where in
      Catalog.set_rows name t (tfilter (fun row -> not (ok row)) (Catalog.rows t));
      Tag (Printf.sprintf "DELETE %d" (before - List.length (Catalog.rows t))))
  | Sql.Update (name, assigns, where) -> (
    match Catalog.find name with
    | None -> raise (Sql_error ("42P01", Printf.sprintf "relation \"%s\" does not exist" name))
    | Some t ->
      let count = ref 0 in
      let ok = compile_cond t where in
      let apply row =
        if ok row then begin
          incr count;
          List.fold_left
            (fun r (col, pv) ->
              let i = col_idx t col in
              Array.mapi (fun j v -> if j = i then coerce (col_typ t col) (deref pv) else v) r)
            row assigns
        end
        else row
      in
      Catalog.set_rows name t (tmap apply (Catalog.rows t));
      Tag (Printf.sprintf "UPDATE %d" !count))
  | Sql.Select sel -> (
    match sel.Sql.from with
    | Sql.Table tname -> run_table ~notice tname (normalize_single tname sel)
    | Sql.Join (a, b, on, k) -> run_join ~notice a b on k sel)
