(* bench.ml — microbenchmarks for every query category the engine supports.

   Times each operation against the executor directly (no wire round-trip), at a
   few data scales, so you can see the access-path and join-algorithm costs the
   planner is choosing between. Run with:  dune exec bench/bench.exe

   Reads the results but discards them; write cost (INSERT) is O(n^2) here because
   each insert rewrites the whole heap file — that is itself one of the numbers
   worth seeing. *)

open Pgwire

let run s = ignore (Exec.run (Sql.parse s))

let query s = match Exec.run (Sql.parse s) with Exec.Rows (_, rows) -> rows | Exec.Tag _ -> []

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  ((Unix.gettimeofday () -. t0) *. 1000., r)

(* average wall-clock ms over [reps] runs *)
let avg_ms reps f =
  let total = ref 0. in
  for _ = 1 to reps do
    let ms, _ = time_ms f in
    total := !total +. ms
  done;
  !total /. float_of_int reps

let fresh () = Hashtbl.reset Catalog.tables

let line cat scale ms note = Printf.printf "  %-30s %8s %10.3f ms   %s\n%!" cat scale ms note
let hr () = print_endline (String.make 78 '-')
let section title = Printf.printf "\n%s\n" title; hr ()

(* populate t(id int, g int, name text) with n rows: id 1..n, g = id mod 10 *)
let populate n =
  fresh ();
  run "CREATE TABLE t (id int, g int, name text)";
  for i = 1 to n do
    run (Printf.sprintf "INSERT INTO t VALUES (%d, %d, 'r%d')" i (i mod 10) i)
  done

(* --- read-path benchmarks at one scale --- *)
let bench_reads n =
  populate n;
  let scale = string_of_int n in
  let reps = if n <= 500 then 50 else if n <= 5000 then 20 else 5 in
  let half = string_of_int (n / 2) in
  (* seq scans first, before any index exists *)
  let seq_pt = avg_ms reps (fun () -> query ("SELECT name FROM t WHERE id = " ^ half)) in
  let seq_rng = avg_ms reps (fun () -> query ("SELECT id FROM t WHERE id > " ^ half)) in
  line "seq scan (point WHERE id=)" scale seq_pt "";
  line "seq scan (range WHERE id>)" scale seq_rng "";
  (* now build an index and repeat the same queries *)
  run "CREATE INDEX ON t (id)";
  let idx_pt = avg_ms reps (fun () -> query ("SELECT name FROM t WHERE id = " ^ half)) in
  let idx_rng = avg_ms reps (fun () -> query ("SELECT id FROM t WHERE id > " ^ half)) in
  line "index scan (point WHERE id=)" scale idx_pt (Printf.sprintf "%.0fx vs seq" (seq_pt /. idx_pt));
  line "index range (WHERE id>)" scale idx_rng (Printf.sprintf "%.1fx vs seq" (seq_rng /. idx_rng));
  (* aggregates / grouping / ordering / distinct *)
  line "aggregate COUNT+SUM+AVG" scale (avg_ms reps (fun () -> query "SELECT COUNT(*), SUM(id), AVG(id) FROM t")) "";
  line "GROUP BY g, COUNT(*)" scale (avg_ms reps (fun () -> query "SELECT g, COUNT(*) FROM t GROUP BY g")) "10 groups";
  line "ORDER BY id DESC LIMIT 10" scale (avg_ms reps (fun () -> query "SELECT id FROM t ORDER BY id DESC LIMIT 10")) "";
  line "SELECT DISTINCT g" scale (avg_ms reps (fun () -> query "SELECT DISTINCT g FROM t")) "";
  line "compound WHERE (AND/OR)" scale (avg_ms reps (fun () -> query "SELECT id FROM t WHERE id > 10 AND g = 5 OR id < 3")) ""

(* --- write path: pure INSERT cost --- *)
let bench_inserts n =
  fresh ();
  run "CREATE TABLE w (id int)";
  let ms, _ = time_ms (fun () -> for i = 1 to n do run (Printf.sprintf "INSERT INTO w VALUES (%d)" i) done) in
  line "INSERT n rows (WAL append)" (string_of_int n) ms
    (Printf.sprintf "%.2f us/row" (ms *. 1000. /. float_of_int n))

(* --- join algorithms: nested loop (tiny) vs hash (larger) --- *)
let bench_join n =
  fresh ();
  run "CREATE TABLE a (id int, k int)";
  run "CREATE TABLE b (k int, v text)";
  for i = 1 to n do run (Printf.sprintf "INSERT INTO a VALUES (%d, %d)" i i) done;
  for i = 1 to n do run (Printf.sprintf "INSERT INTO b VALUES (%d, 'v%d')" i i) done;
  let reps = if n <= 8 then 100 else if n <= 500 then 20 else 5 in
  let ms = avg_ms reps (fun () -> query "SELECT a.id, b.v FROM a JOIN b ON a.k = b.k") in
  let algo = if n > 8 then "hash join O(n+m)" else "nested loop O(n*m)" in
  line "equi-join a x b" (string_of_int n) ms algo

let () =
  Unix.putenv "PGWIRE_DATA" (Filename.concat (Filename.get_temp_dir_name ()) "pgwire_bench");
  (try Sys.readdir (Sys.getenv "PGWIRE_DATA") |> Array.iter (fun f -> Sys.remove (Filename.concat (Sys.getenv "PGWIRE_DATA") f)) with _ -> ());
  Printf.printf "pgwire-ocaml benchmarks  (avg wall-clock per op)\n";

  section "Read paths — seq vs index, aggregates, sort, distinct";
  Printf.printf "  %-30s %8s %13s   %s\n" "operation" "rows" "time" "note";
  hr ();
  List.iter bench_reads [ 2000; 100000 ];

  section "Write path — INSERT (append-only page, O(1) per row)";
  List.iter bench_inserts [ 10000; 100000 ];

  section "Join algorithms — planner picks by size";
  List.iter bench_join [ 5; 1000; 50000 ];

  print_endline "\ndone."
