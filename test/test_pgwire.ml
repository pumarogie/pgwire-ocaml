(* Assert-based tests for the storage, parser, executor, planner and params.
   Run with `dune runtest` (or `dune exec test/test_pgwire.exe`). Any failed
   assertion aborts with a nonzero exit, which dune reports as a test failure. *)

open Pgwire

let ok name = Printf.printf "  ok: %s\n%!" name

let contains s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec go i = i + lsub <= ls && (String.sub s i lsub = sub || go (i + 1)) in
  go 0

let run ?notice s = Exec.run ?notice (Sql.parse s)
let rows_of = function Exec.Rows (_, r) -> r | Exec.Tag _ -> assert false

(* --- storage --- *)

let test_page () =
  let p = Page.create () in
  assert (Page.add p (Bytes.of_string "abc"));
  assert (Page.add p (Bytes.of_string "de"));
  assert (List.map Bytes.to_string (Page.tuples p) = [ "abc"; "de" ]);
  (* survives raw-bytes round-trip (what a heap file stores) *)
  let p2 = Page.of_bytes (Page.to_bytes p) in
  assert (List.map Bytes.to_string (Page.tuples p2) = [ "abc"; "de" ]);
  ok "page: slotted layout + byte round-trip"

let test_tuple_codec () =
  let row = [| Catalog.VInt 42; Catalog.VText "hi"; Catalog.VNull; Catalog.VInt (-7) |] in
  assert (Catalog.decode_row (Catalog.encode_row row) = row);
  ok "tuple: encode/decode incl NULL and negative int"

(* --- parser --- *)

let test_parser () =
  (match Sql.parse "SELECT * FROM t" with
   | Sql.Select { distinct = false; items = [ Sql.Star ]; from = Sql.Table "t"; where = None; group_by = None; having = None; order_by = None; limit = None; offset = None } -> ()
   | _ -> assert false);
  (match Sql.parse "SELECT a.x FROM a JOIN b ON a.k = b.k" with
   | Sql.Select { items = [ Sql.Col "a.x" ]; from = Sql.Join ("a", "b", ("a.k", "b.k"), Sql.Inner); _ } -> ()
   | _ -> assert false);
  (match Sql.parse "SELECT a.x FROM a LEFT JOIN b ON a.k = b.k" with
   | Sql.Select { from = Sql.Join ("a", "b", ("a.k", "b.k"), Sql.Left); _ } -> ()
   | _ -> assert false);
  (match Sql.parse "INSERT INTO t VALUES ($1, $2)" with
   | Sql.Insert ("t", None, [ [ Sql.Param 1; Sql.Param 2 ] ]) -> ()
   | _ -> assert false);
  assert (Sql.parse "CREATE INDEX ON t (a)" = Sql.CreateIndex ("t", "a"));
  (* unknown leading keyword becomes a harmless Other tag *)
  assert (Sql.parse "SET x = 'y'" = Sql.Other "SET");
  (* aggregates / group by / order by / limit *)
  (match Sql.parse "SELECT COUNT(*) FROM t" with
   | Sql.Select { items = [ Sql.Agg (Sql.Count, None) ]; _ } -> ()
   | _ -> assert false);
  (match Sql.parse "SELECT a, SUM(b) FROM t GROUP BY a" with
   | Sql.Select { items = [ Sql.Col "a"; Sql.Agg (Sql.Sum, Some "b") ]; group_by = Some "a"; _ } -> ()
   | _ -> assert false);
  (match Sql.parse "SELECT * FROM t ORDER BY a DESC LIMIT 5" with
   | Sql.Select { order_by = Some { Sql.by = "a"; desc = true; _ }; limit = Some (Sql.Lit (Catalog.VInt 5)); _ } -> ()
   | _ -> assert false);
  ok "parser: select/insert/param/index/aggregate/order/limit/other"

let test_aggregates () =
  ignore (run "CREATE TABLE t_agg (id int, team text, score int)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO t_agg VALUES (1, 'red', 10)";
      "INSERT INTO t_agg VALUES (2, 'red', 30)";
      "INSERT INTO t_agg VALUES (3, 'blue', 20)" ];
  assert (rows_of (run "SELECT COUNT(*) FROM t_agg") = [ [ Some "3" ] ]);
  assert (rows_of (run "SELECT SUM(score) FROM t_agg") = [ [ Some "60" ] ]);
  assert (rows_of (run "SELECT MIN(score), MAX(score) FROM t_agg") = [ [ Some "10"; Some "30" ] ]);
  assert (rows_of (run "SELECT AVG(score) FROM t_agg") = [ [ Some "20" ] ]);
  assert (rows_of (run "SELECT COUNT(*) FROM t_agg WHERE team = 'red'") = [ [ Some "2" ] ]);
  ok "aggregates: count/sum/min/max/avg (+ where)";
  let sort = List.sort compare in
  assert (sort (rows_of (run "SELECT team, COUNT(*) FROM t_agg GROUP BY team"))
          = sort [ [ Some "red"; Some "2" ]; [ Some "blue"; Some "1" ] ]);
  assert (sort (rows_of (run "SELECT team, SUM(score) FROM t_agg GROUP BY team"))
          = sort [ [ Some "red"; Some "40" ]; [ Some "blue"; Some "20" ] ]);
  ok "group by: count + sum per group"

let test_range () =
  (* t_agg: (id,score) = (1,10) (2,30) (3,20) *)
  let ids s =
    List.sort compare (List.map (function [ Some x ] -> x | _ -> assert false) (rows_of (run s)))
  in
  assert (ids "SELECT id FROM t_agg WHERE score > 15" = [ "2"; "3" ]);
  assert (ids "SELECT id FROM t_agg WHERE score >= 20" = [ "2"; "3" ]);
  assert (ids "SELECT id FROM t_agg WHERE score < 20" = [ "1" ]);
  assert (ids "SELECT id FROM t_agg WHERE score <= 20" = [ "1"; "3" ]);
  ok "range: > >= < <= via seq scan";
  (* build an ordered index — results must be identical *)
  ignore (run "CREATE INDEX ON t_agg (score)");
  assert (ids "SELECT id FROM t_agg WHERE score > 15" = [ "2"; "3" ]);
  assert (ids "SELECT id FROM t_agg WHERE score >= 20" = [ "2"; "3" ]);
  assert (ids "SELECT id FROM t_agg WHERE score < 20" = [ "1" ]);
  assert (ids "SELECT id FROM t_agg WHERE score <= 20" = [ "1"; "3" ]);
  assert (rows_of (run "SELECT id FROM t_agg WHERE score = 30") = [ [ Some "2" ] ]);
  (* a SELECTIVE range uses the index; a non-selective one (score > 15 ≈ 75%) is
     left to a seq scan by the cost estimate *)
  let notes = ref [] in
  ignore (run ~notice:(fun m -> notes := m :: !notes) "SELECT id FROM t_agg WHERE score > 25");
  assert (List.exists (fun m -> contains m "index range scan") !notes);
  ok "range: identical results + selective range uses index"

let test_order_limit () =
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score DESC") = [ [ Some "2" ]; [ Some "3" ]; [ Some "1" ] ]);
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score ASC") = [ [ Some "1" ]; [ Some "3" ]; [ Some "2" ] ]);
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score DESC LIMIT 2") = [ [ Some "2" ]; [ Some "3" ] ]);
  assert (List.length (rows_of (run "SELECT id FROM t_agg LIMIT 1")) = 1);
  assert (rows_of (run "SELECT id FROM t_agg WHERE team = 'red' ORDER BY score ASC") = [ [ Some "1" ]; [ Some "2" ] ]);
  ok "order by asc/desc + limit + where combo"

let names_of = function Exec.Rows (desc, _) -> List.map (fun (n, _, _) -> n) desc | Exec.Tag _ -> assert false

let test_aliases () =
  (* parser attaches the alias to the item *)
  (match Sql.parse "SELECT id AS ident FROM t" with
   | Sql.Select { items = [ Sql.Alias (Sql.Col "id", "ident") ]; _ } -> ()
   | _ -> assert false);
  (* alias drives the RowDescription column name; values unchanged *)
  assert (names_of (run "SELECT score AS points FROM t_agg") = [ "points" ]);
  (* alias on an aggregate *)
  let r = run "SELECT COUNT(*) AS n FROM t_agg" in
  assert (names_of r = [ "n" ] && rows_of r = [ [ Some "3" ] ]);
  ok "column aliases: SELECT x AS y renames the output column"

let test_limit_offset () =
  (* t_agg ordered by score asc => ids 1(10), 3(20), 2(30) *)
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score ASC LIMIT 2 OFFSET 1") = [ [ Some "3" ]; [ Some "2" ] ]);
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score ASC OFFSET 2") = [ [ Some "2" ] ]);
  assert (rows_of (run "SELECT id FROM t_agg ORDER BY score ASC LIMIT 1") = [ [ Some "1" ] ]);
  (* parameterized LIMIT: LIMIT $1 supplied via Bind *)
  let stmt = Exec.bind [| Catalog.VInt 2 |] (Sql.parse "SELECT id FROM t_agg ORDER BY score ASC LIMIT $1") in
  assert (rows_of (Exec.run stmt) = [ [ Some "1" ]; [ Some "3" ] ]);
  ok "limit/offset + parameterized LIMIT $1"

(* --- executor + query planner --- *)

let test_exec_and_planner () =
  ignore (run "CREATE TABLE t_ex (id int, name text)");
  ignore (run "INSERT INTO t_ex VALUES (1, 'alice')");
  ignore (run "INSERT INTO t_ex VALUES (2, 'bob')");
  ignore (run "INSERT INTO t_ex VALUES (3, 'alice')");
  assert (List.length (rows_of (run "SELECT * FROM t_ex")) = 3);
  ok "exec: insert + select all";

  (* WHERE with no index -> seq scan, correct row *)
  let notes = ref [] in
  let r = run ~notice:(fun m -> notes := m :: !notes) "SELECT name FROM t_ex WHERE id = 2" in
  assert (rows_of r = [ [ Some "bob" ] ]);
  assert (List.exists (fun m -> contains m "seq scan") !notes);
  ok "planner: seq scan chosen without index";

  (* build an index -> planner switches to index scan, same result *)
  ignore (run "CREATE INDEX ON t_ex (id)");
  let notes = ref [] in
  let r = run ~notice:(fun m -> notes := m :: !notes) "SELECT name FROM t_ex WHERE id = 2" in
  assert (rows_of r = [ [ Some "bob" ] ]);
  assert (List.exists (fun m -> contains m "index scan") !notes);
  ok "planner: index scan chosen after CREATE INDEX (same result)";

  (* index on text column returns ALL matching rows *)
  ignore (run "CREATE INDEX ON t_ex (name)");
  let r = run "SELECT id FROM t_ex WHERE name = 'alice'" in
  assert (List.sort compare (rows_of r) = [ [ Some "1" ]; [ Some "3" ] ]);
  ok "index: returns all matches for duplicate key"

(* --- bind parameters (extended protocol) --- *)

let test_multipage () =
  ignore (run "CREATE TABLE big (id int, name text)");
  for i = 1 to 1000 do
    ignore (run (Printf.sprintf "INSERT INTO big VALUES (%d, 'row%d')" i i))
  done;
  assert (rows_of (run "SELECT COUNT(*) FROM big") = [ [ Some "1000" ] ]);
  (* checkpoint folds the WAL into pages; data then overflows one 8 KB page *)
  Catalog.checkpoint "big";
  let sz = (Unix.stat (Filename.concat (Sys.getenv "PGWIRE_DATA") "big.page")).Unix.st_size in
  assert (sz > Pgwire.Page.page_size);
  (* tail row intact *)
  assert (rows_of (run "SELECT name FROM big WHERE id = 1000") = [ [ Some "row1000" ] ]);
  (* every row survives a reload across all pages *)
  Hashtbl.remove Catalog.tables "big";
  Catalog.load ();
  assert (rows_of (run "SELECT COUNT(*) FROM big") = [ [ Some "1000" ] ]);
  assert (rows_of (run "SELECT name FROM big WHERE id = 777") = [ [ Some "row777" ] ]);
  ok "multi-page: 1000 rows span pages + survive restart"

let test_mutations () =
  ignore (run "CREATE TABLE m (id int, name text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO m VALUES (1, 'a')"; "INSERT INTO m VALUES (2, 'b')"; "INSERT INTO m VALUES (3, 'c')" ];
  (* UPDATE with WHERE *)
  (match run "UPDATE m SET name = 'B' WHERE id = 2" with Exec.Tag "UPDATE 1" -> () | _ -> assert false);
  assert (rows_of (run "SELECT name FROM m WHERE id = 2") = [ [ Some "B" ] ]);
  (* UPDATE whole table *)
  (match run "UPDATE m SET name = 'x'" with Exec.Tag "UPDATE 3" -> () | _ -> assert false);
  assert (List.length (rows_of (run "SELECT * FROM m WHERE name = 'x'")) = 3);
  ok "update: WHERE + full-table";
  (* DELETE with WHERE *)
  (match run "DELETE FROM m WHERE id = 1" with Exec.Tag "DELETE 1" -> () | _ -> assert false);
  assert (rows_of (run "SELECT COUNT(*) FROM m") = [ [ Some "2" ] ]);
  (* an index must stay consistent across a mutation *)
  ignore (run "CREATE INDEX ON m (id)");
  ignore (run "DELETE FROM m WHERE id = 2");
  assert (rows_of (run "SELECT COUNT(*) FROM m") = [ [ Some "1" ] ]);
  let notes = ref [] in
  let r = run ~notice:(fun s -> notes := s :: !notes) "SELECT id FROM m WHERE id = 3" in
  assert (rows_of r = [ [ Some "3" ] ]);
  assert (List.exists (fun s -> contains s "index scan") !notes);
  (* deleted key gone from the index *)
  assert (rows_of (run "SELECT id FROM m WHERE id = 2") = []);
  ok "delete: WHERE + index consistency";
  (* mutations persist across a restart *)
  ignore (run "INSERT INTO m VALUES (9, 'z')");
  Hashtbl.remove Catalog.tables "m";
  Catalog.load ();
  assert (List.sort compare (rows_of (run "SELECT id FROM m")) = [ [ Some "3" ]; [ Some "9" ] ]);
  ok "mutations: persist across restart"

let test_join () =
  ignore (run "CREATE TABLE emp (id int, name text, dept int)");
  ignore (run "CREATE TABLE dept (did int, dname text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO emp VALUES (1, 'ann', 10)";
      "INSERT INTO emp VALUES (2, 'bob', 20)";
      "INSERT INTO emp VALUES (3, 'cy', 10)";
      "INSERT INTO dept VALUES (10, 'eng')";
      "INSERT INTO dept VALUES (20, 'sales')" ];
  let sortr = List.sort compare in
  let r = run "SELECT emp.name, dept.dname FROM emp JOIN dept ON emp.dept = dept.did" in
  assert (sortr (rows_of r) = sortr [ [ Some "ann"; Some "eng" ]; [ Some "bob"; Some "sales" ]; [ Some "cy"; Some "eng" ] ]);
  ok "join: equi-join with qualified projection";
  let r = run "SELECT emp.name FROM emp JOIN dept ON emp.dept = dept.did WHERE dept.dname = 'eng' ORDER BY emp.name DESC" in
  assert (rows_of r = [ [ Some "cy" ]; [ Some "ann" ] ]);
  ok "join: WHERE + ORDER BY on joined columns";
  let r = run "SELECT * FROM emp JOIN dept ON emp.dept = dept.did WHERE emp.id = 1" in
  assert (rows_of r = [ [ Some "1"; Some "ann"; Some "10"; Some "10"; Some "eng" ] ]);
  ok "join: SELECT * spans both tables";
  let r = run "SELECT name, dname FROM emp JOIN dept ON dept = did WHERE id = 2" in
  assert (rows_of r = [ [ Some "bob"; Some "sales" ] ]);
  ok "join: unqualified column resolution"

let test_left_join () =
  (* dan's dept (99) has no matching row in dept *)
  ignore (run "INSERT INTO emp VALUES (4, 'dan', 99)");
  (* INNER join drops dan *)
  let r = run "SELECT emp.name, dept.dname FROM emp JOIN dept ON emp.dept = dept.did" in
  assert (not (List.mem [ Some "dan"; None ] (rows_of r)));
  assert (List.length (rows_of r) = 3);
  (* LEFT join keeps dan with NULL dept columns *)
  let r = run "SELECT emp.name, dept.dname FROM emp LEFT JOIN dept ON emp.dept = dept.did" in
  assert (List.mem [ Some "dan"; None ] (rows_of r));
  assert (List.length (rows_of r) = 4);
  (* unmatched rows found via IS NULL on the right side *)
  let r = run "SELECT emp.name FROM emp LEFT JOIN dept ON emp.dept = dept.did WHERE dept.did IS NULL" in
  assert (rows_of r = [ [ Some "dan" ] ]);
  ok "left join: NULL-pads unmatched left rows; IS NULL finds them"

let test_right_full_join () =
  ignore (run "CREATE TABLE l (id int, rk int)");
  ignore (run "CREATE TABLE r (rk int, name text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO l VALUES (1, 10)"; "INSERT INTO l VALUES (2, 99)"; (* 2 = unmatched left *)
      "INSERT INTO r VALUES (10, 'a')"; "INSERT INTO r VALUES (30, 'b')" ]; (* 30 = unmatched right *)
  let sortr = List.sort compare in
  (* RIGHT: matched (1-a) + unmatched right (null|b); NOT unmatched left (2) *)
  let rr = run "SELECT l.id, r.name FROM l RIGHT JOIN r ON l.rk = r.rk" in
  assert (sortr (rows_of rr) = sortr [ [ Some "1"; Some "a" ]; [ None; Some "b" ] ]);
  ok "right join: NULL-pads unmatched right rows";
  (* FULL: matched + unmatched left (2|null) + unmatched right (null|b) *)
  let fr = run "SELECT l.id, r.name FROM l FULL JOIN r ON l.rk = r.rk" in
  assert (sortr (rows_of fr) = sortr [ [ Some "1"; Some "a" ]; [ Some "2"; None ]; [ None; Some "b" ] ]);
  ok "full join: keeps unmatched rows from both sides"

let test_hash_join () =
  ignore (run "CREATE TABLE big1 (id int, k int)");
  ignore (run "CREATE TABLE big2 (k int, tag text)");
  for i = 1 to 10 do ignore (run (Printf.sprintf "INSERT INTO big1 VALUES (%d, %d)" i i)) done;
  for i = 5 to 14 do ignore (run (Printf.sprintf "INSERT INTO big2 VALUES (%d, 't')" i)) done;
  (* keys: big1 1..10, big2 5..14 -> 6 matched, 4 unmatched each side *)
  let notes = ref [] in
  let r = run ~notice:(fun m -> notes := m :: !notes) "SELECT big1.id FROM big1 JOIN big2 ON big1.k = big2.k" in
  assert (List.exists (fun m -> contains m "hash join") !notes);
  assert (List.length (rows_of r) = 6);
  assert (List.length (rows_of (run "SELECT big1.id FROM big1 LEFT JOIN big2 ON big1.k = big2.k")) = 10);
  assert (List.length (rows_of (run "SELECT big1.id FROM big1 RIGHT JOIN big2 ON big1.k = big2.k")) = 10);
  assert (List.length (rows_of (run "SELECT big1.id FROM big1 FULL JOIN big2 ON big1.k = big2.k")) = 14);
  ok "hash join: chosen for large inputs; INNER/LEFT/RIGHT/FULL all correct";
  (* small tables still use nested loop *)
  ignore (run "CREATE TABLE s1 (k int)");
  ignore (run "CREATE TABLE s2 (k int)");
  ignore (run "INSERT INTO s1 VALUES (1)");
  ignore (run "INSERT INTO s2 VALUES (1)");
  let notes = ref [] in
  ignore (run ~notice:(fun m -> notes := m :: !notes) "SELECT s1.k FROM s1 JOIN s2 ON s1.k = s2.k");
  assert (List.exists (fun m -> contains m "nested loop") !notes);
  ok "hash join: small inputs still use nested loop";
  (* NULL join keys never match (both algorithms), SQL semantics *)
  ignore (run "INSERT INTO s1 VALUES (null)");
  ignore (run "INSERT INTO s2 VALUES (null)");
  assert (List.length (rows_of (run "SELECT s1.k FROM s1 JOIN s2 ON s1.k = s2.k")) = 1);
  assert (List.length (rows_of (run "SELECT s1.k FROM s1 LEFT JOIN s2 ON s1.k = s2.k")) = 2);
  ok "join: NULL join keys do not match"

let test_transactions () =
  ignore (run "CREATE TABLE acct (id int, bal int)");
  ignore (run "INSERT INTO acct VALUES (1, 100)");
  (* ROLLBACK restores rows changed during the transaction *)
  let snap = Catalog.snapshot () in
  ignore (run "INSERT INTO acct VALUES (2, 200)");
  ignore (run "UPDATE acct SET bal = 0 WHERE id = 1");
  assert (rows_of (run "SELECT COUNT(*) FROM acct") = [ [ Some "2" ] ]);
  Catalog.restore snap;
  assert (rows_of (run "SELECT COUNT(*) FROM acct") = [ [ Some "1" ] ]);
  assert (rows_of (run "SELECT bal FROM acct WHERE id = 1") = [ [ Some "100" ] ]);
  ok "transaction: ROLLBACK restores rows";
  (* ROLLBACK drops a table created inside the transaction *)
  let snap = Catalog.snapshot () in
  ignore (run "CREATE TABLE temp (x int)");
  ignore (run "INSERT INTO temp VALUES (9)");
  assert (Catalog.find "temp" <> None);
  Catalog.restore snap;
  assert (Catalog.find "temp" = None);
  ok "transaction: ROLLBACK drops table created in txn";
  (* COMMIT = discard the snapshot; changes stay *)
  ignore (run "INSERT INTO acct VALUES (3, 300)");
  let _snap = Catalog.snapshot () in
  assert (rows_of (run "SELECT COUNT(*) FROM acct") = [ [ Some "2" ] ]);
  ok "transaction: COMMIT keeps changes"

let test_nulls () =
  ignore (run "CREATE TABLE nl (id int, note text)");
  ignore (run "INSERT INTO nl VALUES (1, 'x')");
  ignore (run "INSERT INTO nl VALUES (2, null)");
  ignore (run "INSERT INTO nl VALUES (3, null)");
  let ids s = List.sort compare (rows_of (run s)) in
  (* = NULL matches nothing (three-valued logic) *)
  assert (rows_of (run "SELECT id FROM nl WHERE note = 'x'") = [ [ Some "1" ] ]);
  assert (rows_of (run "SELECT id FROM nl WHERE note = null") = []);
  (* IS NULL / IS NOT NULL *)
  assert (ids "SELECT id FROM nl WHERE note IS NULL" = [ [ Some "2" ]; [ Some "3" ] ]);
  assert (ids "SELECT id FROM nl WHERE note IS NOT NULL" = [ [ Some "1" ] ]);
  (* comparisons exclude NULL column values *)
  assert (rows_of (run "SELECT id FROM nl WHERE note > 'a'") = [ [ Some "1" ] ]);
  ok "null: = NULL matches nothing; IS [NOT] NULL; comparisons skip NULL";
  (* aggregates: COUNT of a column skips NULL, COUNT star counts all rows *)
  assert (rows_of (run "SELECT COUNT(note) FROM nl") = [ [ Some "1" ] ]);
  assert (rows_of (run "SELECT COUNT(*) FROM nl") = [ [ Some "3" ] ]);
  (* DELETE with IS NULL *)
  ignore (run "DELETE FROM nl WHERE note IS NULL");
  assert (rows_of (run "SELECT COUNT(*) FROM nl") = [ [ Some "1" ] ]);
  ok "null: COUNT semantics + DELETE WHERE IS NULL"

let test_types () =
  ignore (run "CREATE TABLE prod (id int, name text, price float, active bool)");
  ignore (run "INSERT INTO prod VALUES (1, 'a', 9.99, true)");
  ignore (run "INSERT INTO prod VALUES (2, 'b', 19.5, false)");
  ignore (run "INSERT INTO prod VALUES (3, 'c', 5.0, true)");
  (* bool renders as t/f *)
  assert (rows_of (run "SELECT active FROM prod WHERE id = 1") = [ [ Some "t" ] ]);
  assert (rows_of (run "SELECT active FROM prod WHERE id = 2") = [ [ Some "f" ] ]);
  let ids s = List.sort compare (rows_of (run s)) in
  (* filter on bool *)
  assert (ids "SELECT id FROM prod WHERE active = true" = [ [ Some "1" ]; [ Some "3" ] ]);
  (* float compare, incl. int literal vs float column *)
  assert (ids "SELECT id FROM prod WHERE price > 6" = [ [ Some "1" ]; [ Some "2" ] ]);
  assert (rows_of (run "SELECT id FROM prod WHERE price < 6") = [ [ Some "3" ] ]);
  ok "types: bool + float insert / select / filter";
  (* survive restart *)
  Hashtbl.remove Catalog.tables "prod";
  Catalog.load ();
  assert (rows_of (run "SELECT price, active FROM prod WHERE id = 1") = [ [ Some "9.99"; Some "t" ] ]);
  ok "types: bool + float persist across restart"

let test_numeric_agg () =
  ignore (run "CREATE TABLE sales (region text, amt float)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO sales VALUES ('n', 10.5)";
      "INSERT INTO sales VALUES ('n', 4.5)";
      "INSERT INTO sales VALUES ('s', 20.0)" ];
  assert (rows_of (run "SELECT SUM(amt) FROM sales") = [ [ Some "35" ] ]);
  assert (rows_of (run "SELECT MIN(amt), MAX(amt) FROM sales") = [ [ Some "4.5"; Some "20" ] ]);
  assert (rows_of (run "SELECT AVG(amt) FROM sales") = [ [ Some "11.6667" ] ]);
  (* AVG of ints is now fractional, not floored *)
  ignore (run "CREATE TABLE g (n int)");
  ignore (run "INSERT INTO g VALUES (1)");
  ignore (run "INSERT INTO g VALUES (2)");
  assert (rows_of (run "SELECT AVG(n) FROM g") = [ [ Some "1.5" ] ]);
  (* SUM stays exact-int for int columns *)
  assert (rows_of (run "SELECT SUM(n) FROM g") = [ [ Some "3" ] ]);
  (* GROUP BY with float SUM *)
  let sortr = List.sort compare in
  assert (sortr (rows_of (run "SELECT region, SUM(amt) FROM sales GROUP BY region"))
          = sortr [ [ Some "n"; Some "15" ]; [ Some "s"; Some "20" ] ]);
  ok "numeric aggregation: SUM/MIN/MAX/AVG over floats + fractional AVG"

let test_distinct () =
  ignore (run "CREATE TABLE c (id int, color text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO c VALUES (1, 'red')"; "INSERT INTO c VALUES (2, 'red')"; "INSERT INTO c VALUES (3, 'blue')" ];
  let sortr = List.sort compare in
  assert (List.length (rows_of (run "SELECT color FROM c")) = 3);
  assert (sortr (rows_of (run "SELECT DISTINCT color FROM c")) = sortr [ [ Some "red" ]; [ Some "blue" ] ]);
  (* DISTINCT combines with ORDER BY (dedupe before limit) *)
  assert (rows_of (run "SELECT DISTINCT color FROM c ORDER BY color") = [ [ Some "blue" ]; [ Some "red" ] ]);
  assert (rows_of (run "SELECT DISTINCT color FROM c ORDER BY color LIMIT 1") = [ [ Some "blue" ] ]);
  ok "distinct: dedupes result rows (with order + limit)"

let test_having () =
  ignore (run "CREATE TABLE o (cust text, amt int)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO o VALUES ('a', 10)"; "INSERT INTO o VALUES ('a', 20)";
      "INSERT INTO o VALUES ('b', 5)"; "INSERT INTO o VALUES ('c', 7)";
      "INSERT INTO o VALUES ('c', 8)" ];
  (* a: 2 orders / 30, b: 1 / 5, c: 2 / 15 *)
  let sortr = List.sort compare in
  assert (sortr (rows_of (run "SELECT cust FROM o GROUP BY cust HAVING COUNT(*) > 1"))
          = sortr [ [ Some "a" ]; [ Some "c" ] ]);
  assert (rows_of (run "SELECT cust, SUM(amt) FROM o GROUP BY cust HAVING SUM(amt) >= 20")
          = [ [ Some "a"; Some "30" ] ]);
  ok "having: filters groups by an aggregate"

let test_compound_where () =
  ignore (run "CREATE TABLE p (id int, age int, city text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO p VALUES (1, 30, 'ny')"; "INSERT INTO p VALUES (2, 20, 'ny')";
      "INSERT INTO p VALUES (3, 40, 'la')"; "INSERT INTO p VALUES (4, 25, 'la')" ];
  let ids s = List.sort compare (rows_of (run s)) in
  assert (ids "SELECT id FROM p WHERE age > 25 AND city = 'ny'" = [ [ Some "1" ] ]);
  assert (ids "SELECT id FROM p WHERE city = 'la' OR age < 22" = [ [ Some "2" ]; [ Some "3" ]; [ Some "4" ] ]);
  (* AND binds tighter than OR *)
  assert (ids "SELECT id FROM p WHERE city = 'ny' AND age > 25 OR city = 'la'" = [ [ Some "1" ]; [ Some "3" ]; [ Some "4" ] ]);
  (* parens override precedence *)
  assert (ids "SELECT id FROM p WHERE city = 'ny' AND (age > 35 OR age < 22)" = [ [ Some "2" ] ]);
  ok "compound where: AND / OR / precedence / parens"

let test_review_fixes () =
  (* #6 negative numeric literals *)
  ignore (run "CREATE TABLE neg (n int, f float)");
  ignore (run "INSERT INTO neg VALUES (-5, -2.5)");
  assert (rows_of (run "SELECT n, f FROM neg WHERE n = -5") = [ [ Some "-5"; Some "-2.5" ] ]);
  assert (rows_of (run "SELECT n FROM neg WHERE n < -1") = [ [ Some "-5" ] ]);
  ok "fix: negative numeric literals";
  (* #3 aggregates over zero input rows return NULL (COUNT returns 0) *)
  ignore (run "CREATE TABLE em (x int)");
  assert (rows_of (run "SELECT SUM(x), MIN(x), MAX(x), AVG(x), COUNT(*) FROM em")
          = [ [ None; None; None; None; Some "0" ] ]);
  ok "fix: empty-group aggregates return NULL";
  (* #4 aggregate ORDER BY + LIMIT *)
  ignore (run "CREATE TABLE ag (g int, v int)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO ag VALUES (3, 1)"; "INSERT INTO ag VALUES (1, 1)"; "INSERT INTO ag VALUES (2, 1)"; "INSERT INTO ag VALUES (1, 1)" ];
  assert (rows_of (run "SELECT g, COUNT(*) FROM ag GROUP BY g ORDER BY g")
          = [ [ Some "1"; Some "2" ]; [ Some "2"; Some "1" ]; [ Some "3"; Some "1" ] ]);
  assert (rows_of (run "SELECT g FROM ag GROUP BY g ORDER BY g DESC LIMIT 2") = [ [ Some "3" ]; [ Some "2" ] ]);
  ok "fix: aggregate ORDER BY + LIMIT";
  (* #2 index range scan agrees with seq scan on NULLs *)
  ignore (run "CREATE TABLE ir (k int)");
  List.iter (fun s -> ignore (run s)) [ "INSERT INTO ir VALUES (1)"; "INSERT INTO ir VALUES (5)"; "INSERT INTO ir VALUES (null)" ];
  let seq = List.sort compare (rows_of (run "SELECT k FROM ir WHERE k < 10")) in
  ignore (run "CREATE INDEX ON ir (k)");
  let idx = List.sort compare (rows_of (run "SELECT k FROM ir WHERE k < 10")) in
  assert (seq = idx && seq = [ [ Some "1" ]; [ Some "5" ] ]);
  ok "fix: index range scan excludes NULL like seq scan";
  (* #5 value coerced to column type on insert *)
  ignore (run "CREATE TABLE co (s text, n int)");
  ignore (run "INSERT INTO co VALUES (10, 5)");
  assert (rows_of (run "SELECT s FROM co WHERE s = '10'") = [ [ Some "10" ] ]);
  ok "fix: insert coerces value to column type";
  (* #9 qualified aggregate argument *)
  assert (rows_of (run "SELECT SUM(ag.v) FROM ag") = [ [ Some "4" ] ]);
  ok "fix: qualified aggregate argument parses";
  (* #10 ROLLBACK reverts an index created inside the transaction *)
  ignore (run "CREATE TABLE rx (a int)");
  ignore (run "INSERT INTO rx VALUES (1)");
  let snap = Catalog.snapshot () in
  ignore (run "CREATE INDEX ON rx (a)");
  assert (Catalog.find_index (Option.get (Catalog.find "rx")) "a" <> None);
  Catalog.restore snap;
  assert (Catalog.find_index (Option.get (Catalog.find "rx")) "a" = None);
  ok "fix: ROLLBACK drops index created in transaction"

let test_index_order () =
  ignore (run "CREATE TABLE io (id int, name text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO io VALUES (3, 'c')"; "INSERT INTO io VALUES (1, 'a')"; "INSERT INTO io VALUES (2, 'b')" ];
  let asc = [ [ Some "1" ]; [ Some "2" ]; [ Some "3" ] ] in
  assert (rows_of (run "SELECT id FROM io ORDER BY id") = asc);
  (* build index -> ORDER BY should use the ordered scan, same result *)
  ignore (run "CREATE INDEX ON io (id)");
  let notes = ref [] in
  let r = run ~notice:(fun m -> notes := m :: !notes) "SELECT id FROM io ORDER BY id" in
  assert (rows_of r = asc);
  assert (List.exists (fun m -> contains m "index ordered scan") !notes);
  assert (rows_of (run "SELECT id FROM io ORDER BY id DESC") = [ [ Some "3" ]; [ Some "2" ]; [ Some "1" ] ]);
  assert (rows_of (run "SELECT id FROM io ORDER BY id DESC LIMIT 2") = [ [ Some "3" ]; [ Some "2" ] ]);
  ok "index-ordered scan: ORDER BY on indexed column skips sort, same result"

let test_large_result () =
  ignore (run "CREATE TABLE lr (id int)");
  for i = 1 to 20000 do ignore (run (Printf.sprintf "INSERT INTO lr VALUES (%d)" i)) done;
  let r = rows_of (run "SELECT id FROM lr") in
  assert (List.length r = 20000);
  assert (List.hd r = [ Some "1" ] && List.nth r 19999 = [ Some "20000" ]); (* order preserved *)
  assert (List.length (rows_of (run "SELECT id FROM lr WHERE id > 5000")) = 15000);
  ok "large result: 20k rows project/filter tail-safely, order preserved"

let test_cost_planner () =
  ignore (run "CREATE TABLE cp (id int)");
  for i = 1 to 1000 do ignore (run (Printf.sprintf "INSERT INTO cp VALUES (%d)" i)) done;
  ignore (run "CREATE INDEX ON cp (id)");
  let plan s = let n = ref [] in ignore (run ~notice:(fun m -> n := m :: !n) s); String.concat "|" !n in
  (* selective range (id > 990 ≈ 1%) -> index; non-selective (id > 100 ≈ 90%) -> seq *)
  assert (contains (plan "SELECT id FROM cp WHERE id > 990") "index range");
  assert (contains (plan "SELECT id FROM cp WHERE id > 100") "seq scan");
  (* correct results regardless of plan *)
  assert (List.length (rows_of (run "SELECT id FROM cp WHERE id > 990")) = 10);
  assert (List.length (rows_of (run "SELECT id FROM cp WHERE id > 100")) = 900);
  ok "cost planner: selective range uses index, non-selective uses seq"

(* ===== batch of 10 SQL features (one PR) ===== *)

let names_of = function Exec.Rows (desc, _) -> List.map (fun (n, _, _) -> n) desc | Exec.Tag _ -> assert false
let tag_of = function Exec.Tag t -> t | Exec.Rows _ -> assert false
let _ = (names_of, tag_of) (* used by later tests in this batch *)

(* #6 — float rounds to nearest int on insert into an int column (PG behavior) *)
let test_coerce_round () =
  ignore (run "CREATE TABLE cr (i int)");
  ignore (run "INSERT INTO cr VALUES (9.99)");
  ignore (run "INSERT INTO cr VALUES (2.4)");
  assert (List.sort compare (rows_of (run "SELECT i FROM cr")) = [ [ Some "10" ]; [ Some "2" ] ]);
  ok "coerce: float rounds to nearest int, not truncate (#6)"

(* #31 — double-quoted identifiers preserve case (unquoted still folds lower) *)
let test_quoted_idents () =
  ignore (run "CREATE TABLE \"My_T\" (\"Col\" int)");
  ignore (run "INSERT INTO \"My_T\" VALUES (5)");
  assert (rows_of (run "SELECT \"Col\" FROM \"My_T\"") = [ [ Some "5" ] ]);
  ok "quoted identifiers preserve case (#31)"

(* #17 — DROP TABLE and TRUNCATE *)
let test_drop_truncate () =
  ignore (run "CREATE TABLE dt (id int)");
  ignore (run "INSERT INTO dt VALUES (1)");
  ignore (run "INSERT INTO dt VALUES (2)");
  assert (tag_of (run "TRUNCATE dt") = "TRUNCATE TABLE");
  assert (rows_of (run "SELECT id FROM dt") = []);
  assert (tag_of (run "DROP TABLE dt") = "DROP TABLE");
  assert (Catalog.find "dt" = None);
  (try ignore (run "DROP TABLE nope"); assert false with Exec.Sql_error ("42P01", _) -> ());
  assert (tag_of (run "DROP TABLE IF EXISTS nope") = "DROP TABLE");
  ok "DROP TABLE + TRUNCATE, IF EXISTS (#17)"

(* #30 — EXPLAIN returns the planner's access path as rows *)
let test_explain () =
  ignore (run "CREATE TABLE ex (id int)");
  ignore (run "INSERT INTO ex VALUES (1)");
  assert (names_of (run "EXPLAIN SELECT * FROM ex") = [ "QUERY PLAN" ]);
  let rows = rows_of (run "EXPLAIN SELECT * FROM ex") in
  assert (List.exists (function [ Some m ] -> contains m "seq scan" | _ -> false) rows);
  (try ignore (run "EXPLAIN DELETE FROM ex"); assert false with Exec.Sql_error ("0A000", _) -> ());
  ok "EXPLAIN shows the access path, SELECT only (#30)"

(* #19 — MIN/MAX over text (and grouped) *)
let test_text_minmax () =
  ignore (run "CREATE TABLE tm (grp text, name text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO tm VALUES ('a', 'banana')"; "INSERT INTO tm VALUES ('a', 'apple')"; "INSERT INTO tm VALUES ('b', 'cherry')" ];
  assert (rows_of (run "SELECT MIN(name), MAX(name) FROM tm") = [ [ Some "apple"; Some "cherry" ] ]);
  assert (List.sort compare (rows_of (run "SELECT grp, MAX(name) FROM tm GROUP BY grp"))
          = List.sort compare [ [ Some "a"; Some "banana" ]; [ Some "b"; Some "cherry" ] ]);
  ok "text MIN/MAX incl GROUP BY (#19)"

(* #15 (subset) — not-equal operators <> and != *)
let test_not_equal () =
  ignore (run "CREATE TABLE ne (id int)");
  List.iter (fun s -> ignore (run s)) [ "INSERT INTO ne VALUES (1)"; "INSERT INTO ne VALUES (2)"; "INSERT INTO ne VALUES (3)" ];
  let ids s = List.sort compare (rows_of (run s)) in
  assert (ids "SELECT id FROM ne WHERE id <> 2" = [ [ Some "1" ]; [ Some "3" ] ]);
  assert (ids "SELECT id FROM ne WHERE id != 2" = [ [ Some "1" ]; [ Some "3" ] ]);
  (* still correct when an index exists (must not use the index range path) *)
  ignore (run "CREATE INDEX ON ne (id)");
  assert (ids "SELECT id FROM ne WHERE id <> 2" = [ [ Some "1" ]; [ Some "3" ] ]);
  ok "not-equal <> and != (#15)"

(* #33 — string concatenation operator || *)
let test_concat () =
  ignore (run "CREATE TABLE cc (first text, last text)");
  ignore (run "INSERT INTO cc VALUES ('ada', 'lovelace')");
  ignore (run "INSERT INTO cc VALUES ('alan', null)");
  (* column || literal || column *)
  assert (rows_of (run "SELECT first || ' ' || last FROM cc WHERE first = 'ada'") = [ [ Some "ada lovelace" ] ]);
  (* NULL operand makes the whole result NULL (PG semantics) *)
  assert (rows_of (run "SELECT first || last FROM cc WHERE first = 'alan'") = [ [ None ] ]);
  ok "string concatenation || (#33)"

(* #20 — multi-row INSERT and column lists *)
let test_multirow_insert () =
  ignore (run "CREATE TABLE mi (a int, b int, c text)");
  (* multi-row, positional *)
  assert (tag_of (run "INSERT INTO mi VALUES (1, 10, 'x'), (2, 20, 'y')") = "INSERT 0 2");
  (* column list, reordered, missing column defaults to NULL *)
  assert (tag_of (run "INSERT INTO mi (c, a) VALUES ('z', 3)") = "INSERT 0 1");
  assert (rows_of (run "SELECT a, b, c FROM mi WHERE a = 3") = [ [ Some "3"; None; Some "z" ] ]);
  assert (List.length (rows_of (run "SELECT a FROM mi")) = 3);
  ok "multi-row INSERT + column list (#20)"

(* #32 — ORDER BY ordinal and NULLS FIRST/LAST *)
let test_order_ordinal_nulls () =
  ignore (run "CREATE TABLE oo (a int, b text)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO oo VALUES (3, 'c')"; "INSERT INTO oo VALUES (1, 'a')"; "INSERT INTO oo VALUES (2, null)" ];
  (* ORDER BY 1 == ORDER BY the first select item (a) *)
  assert (rows_of (run "SELECT a FROM oo ORDER BY 1") = [ [ Some "1" ]; [ Some "2" ]; [ Some "3" ] ]);
  (* explicit NULLS placement on b (a=2 has NULL b) *)
  assert (rows_of (run "SELECT a FROM oo ORDER BY b NULLS FIRST") = [ [ Some "2" ]; [ Some "1" ]; [ Some "3" ] ]);
  assert (rows_of (run "SELECT a FROM oo ORDER BY b NULLS LAST") = [ [ Some "1" ]; [ Some "3" ]; [ Some "2" ] ]);
  ok "ORDER BY ordinal + NULLS FIRST/LAST (#32)"

(* #3 — HAVING with AND/OR (previously accepted only one comparison) *)
let test_having_andor () =
  ignore (run "CREATE TABLE hv (grp text, n int)");
  List.iter (fun s -> ignore (run s))
    [ "INSERT INTO hv VALUES ('a', 1)"; "INSERT INTO hv VALUES ('a', 2)";
      "INSERT INTO hv VALUES ('b', 10)";
      "INSERT INTO hv VALUES ('c', 4)"; "INSERT INTO hv VALUES ('c', 5)" ];
  let g s = List.sort compare (List.map (function [ Some x ] -> x | _ -> assert false) (rows_of (run s))) in
  (* a: count2 sum3 · b: count1 sum10 · c: count2 sum9 *)
  assert (g "SELECT grp FROM hv GROUP BY grp HAVING COUNT(*) > 1 AND SUM(n) > 5" = [ "c" ]);
  assert (g "SELECT grp FROM hv GROUP BY grp HAVING COUNT(*) > 1 OR SUM(n) > 8" = [ "a"; "b"; "c" ]);
  ok "HAVING with AND/OR (#3)"

let test_comments () =
  (* line comment to end of input *)
  (match Sql.parse "SELECT id FROM t -- trailing comment" with
   | Sql.Select { items = [ Sql.Col "id" ]; from = Sql.Table "t"; _ } -> ()
   | _ -> assert false);
  (* block comment mid-statement *)
  (match Sql.parse "SELECT /* inline */ id FROM t" with
   | Sql.Select { items = [ Sql.Col "id" ]; from = Sql.Table "t"; _ } -> ()
   | _ -> assert false);
  (* line comment before a newline, statement continues *)
  (match Sql.parse "SELECT id -- pick id\nFROM t" with
   | Sql.Select { from = Sql.Table "t"; _ } -> ()
   | _ -> assert false);
  ok "sql comments: line (--) and block (/* */) skipped"

let test_select_no_from () =
  (* liveness/introspection probe: SELECT <const> with no FROM -> one row *)
  assert (rows_of (run "SELECT 1") = [ [ Some "1" ] ]);
  assert (rows_of (run "SELECT 1, 2, 3") = [ [ Some "1"; Some "2"; Some "3" ] ]);
  assert (rows_of (run "SELECT 'ok'") = [ [ Some "ok" ] ]);
  ok "select: constant expression without FROM (SELECT 1)"

let test_params () =
  ignore (run "CREATE TABLE t_p (a int, b text)");
  let ins = Exec.bind [| Catalog.VInt 5; Catalog.VText "x" |] (Sql.parse "INSERT INTO t_p VALUES ($1, $2)") in
  ignore (Exec.run ins);
  let sel = Exec.bind [| Catalog.VInt 5 |] (Sql.parse "SELECT b FROM t_p WHERE a = $1") in
  assert (rows_of (Exec.run sel) = [ [ Some "x" ] ]);
  assert (Exec.count_params (Sql.parse "SELECT * FROM t_p WHERE a = $1") = 1);
  assert (Exec.count_params (Sql.parse "SELECT * FROM t_p") = 0);
  ok "params: bind substitution + count_params"

(* --- errors map to SQLSTATE codes --- *)

let expect_error code thunk =
  match thunk () with
  | exception Exec.Sql_error (c, _) -> assert (c = code)
  | _ -> assert false

let test_errors () =
  expect_error "42P01" (fun () -> run "SELECT * FROM nope");
  expect_error "42703" (fun () -> run "SELECT bad FROM t_ex");
  expect_error "42P01" (fun () -> run "INSERT INTO nope VALUES (1)");
  expect_error "42601" (fun () -> run "INSERT INTO t_ex VALUES (1)");
  (* wrong value count *)
  ok "errors: 42P01 / 42703 / 42601 raised correctly"

(* --- persistence: write, drop from memory, reload from disk --- *)

let test_persistence () =
  ignore (run "CREATE TABLE t_persist (id int, name text)");
  ignore (run "INSERT INTO t_persist VALUES (7, 'zoe')");
  Hashtbl.remove Catalog.tables "t_persist";
  (* simulate a restart *)
  assert (Catalog.find "t_persist" = None);
  Catalog.load ();
  (match Catalog.find "t_persist" with
   | Some t -> assert ((Catalog.rows t) = [ [| Catalog.VInt 7; Catalog.VText "zoe" |] ])
   | None -> assert false);
  ok "persistence: rows reload from disk after restart"

let () =
  (* isolate test data on disk *)
  Unix.putenv "PGWIRE_DATA" (Filename.concat (Filename.get_temp_dir_name ()) "pgwire_test_data");
  (try Sys.readdir (Sys.getenv "PGWIRE_DATA") |> Array.iter (fun f ->
         Sys.remove (Filename.concat (Sys.getenv "PGWIRE_DATA") f))
   with _ -> ());
  print_endline "pgwire tests:";
  test_page ();
  test_tuple_codec ();
  test_parser ();
  test_exec_and_planner ();
  test_aggregates ();
  test_range ();
  test_order_limit ();
  test_limit_offset ();
  test_multipage ();
  test_mutations ();
  test_join ();
  test_left_join ();
  test_right_full_join ();
  test_hash_join ();
  test_transactions ();
  test_nulls ();
  test_types ();
  test_numeric_agg ();
  test_distinct ();
  test_having ();
  test_compound_where ();
  test_review_fixes ();
  test_index_order ();
  test_large_result ();
  test_cost_planner ();
  test_comments ();
  test_coerce_round ();
  test_quoted_idents ();
  test_drop_truncate ();
  test_explain ();
  test_text_minmax ();
  test_not_equal ();
  test_concat ();
  test_multirow_insert ();
  test_order_ordinal_nulls ();
  test_having_andor ();
  test_select_no_from ();
  test_aliases ();
  test_params ();
  test_errors ();
  test_persistence ();
  print_endline "ALL TESTS PASSED"
