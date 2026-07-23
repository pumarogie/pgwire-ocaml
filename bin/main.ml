(* main.ml — TCP server + per-connection session driver.

   `main`          -> listen on 127.0.0.1:5433, serve one client at a time.
   `main test`     -> run module self-checks and exit.

   ponytail: single-client blocking accept loop; move to eio fibers for
   concurrency once the protocol is solid (plan milestone M5). *)

open Pgwire

let port = try int_of_string (Sys.getenv "PGWIRE_PORT") with _ -> 5434

let ssl_request = 80877103
let gss_request = 80877104

(* ParameterStatus values psql likes to see at startup. *)
let startup_params =
  [
    ("server_version", "14.0 (ocaml-pgwire)");
    ("client_encoding", "UTF8");
    ("DateStyle", "ISO, MDY");
    ("standard_conforming_strings", "on");
    ("integer_datetimes", "on");
  ]

let send oc s = output_string oc s

(* Per-connection transaction state. Aborted carries the snapshot so COMMIT after
   an error still rolls back (matching PostgreSQL's aborted-transaction rule). *)
type txnstate = Idle | Active of Catalog.snapshot | Aborted of Catalog.snapshot

let txn_status = function Idle -> 'I' | Active _ -> 'T' | Aborted _ -> 'E'

(* ponytail: one global lock. A transaction HOLDS it from BEGIN through
   COMMIT/ROLLBACK, so concurrent connections cannot interleave writes or clobber
   its whole-catalog snapshot — coarse but correct serialization. Per-row locks +
   MVCC would be the real-database upgrade. *)
let catalog_lock = Mutex.create ()

(* Run [f] under the catalog lock. If a transaction already holds the lock (state
   not Idle) don't re-lock (Mutex isn't reentrant); release only once the
   transaction has ended (state back to Idle). *)
let with_txn_lock tx f =
  if !tx = Idle then Mutex.lock catalog_lock;
  Fun.protect ~finally:(fun () -> if !tx = Idle then Mutex.unlock catalog_lock) f

(* On error inside an active transaction, move it to the aborted state so further
   statements are rejected until COMMIT/ROLLBACK. *)
let abort_if_active tx = match !tx with Active s -> tx := Aborted s | _ -> ()

(* Transaction control is per-connection session state, so it lives here rather
   than in the (stateless, shared) executor. *)
let handle_stmt tx notice stmt =
  match stmt with
  | Sql.Other ("BEGIN" | "START") ->
    (match !tx with Idle -> tx := Active (Catalog.snapshot ()) | _ -> ());
    Exec.Tag "BEGIN"
  | Sql.Other ("COMMIT" | "END") ->
    (match !tx with Aborted s -> Catalog.restore s | _ -> Catalog.fsync_all () (* durability point *));
    tx := Idle;
    Exec.Tag "COMMIT"
  | Sql.Other ("ROLLBACK" | "ABORT") ->
    (match !tx with Active s | Aborted s -> Catalog.restore s | Idle -> ());
    tx := Idle;
    Exec.Tag "ROLLBACK"
  | other -> (
    match !tx with
    | Aborted _ ->
      raise (Exec.Sql_error ("25P02", "current transaction is aborted, commands ignored until end of transaction block"))
    | _ -> Exec.run ~notice other)

(* ponytail: bind params arrive as text; type them by looks (int if it parses as
   one, else text). Real PG uses the parameter type OIDs from Parse/Bind. *)
let value_of_text = function
  | None -> Catalog.VNull
  | Some s -> (
    match int_of_string_opt s with
    | Some n -> Catalog.VInt n
    | None -> (
      match float_of_string_opt s with
      | Some f -> Catalog.VFloat f
      | None -> (
        match s with
        | "t" | "true" -> Catalog.VBool true
        | "f" | "false" -> Catalog.VBool false
        | _ -> Catalog.VText s)))

(* statements that mutate data — an autocommit one is fsynced when it completes *)
let is_write = function
  | Sql.Insert _ | Sql.Delete _ | Sql.Update _ | Sql.Create _ | Sql.CreateIndex _ -> true
  | _ -> false

let handle_query oc tx sql =
  (* strip trailing NUL from the C-string, then whitespace *)
  let sql =
    if String.length sql > 0 && sql.[String.length sql - 1] = '\000' then
      String.sub sql 0 (String.length sql - 1)
    else sql
  in
  let trimmed = String.trim sql in
  if trimmed = "" then send oc (Protocol.empty_query_response ())
  else (
    try
      (* ponytail: parses/runs one statement; multi-statement queries run only the first. *)
      let notice m = send oc (Protocol.notice_response m) in
      let stmt = Sql.parse trimmed in
      (* fsync an autocommit write inside the lock (fsync_all iterates the global
         tables Hashtbl; a concurrent CREATE would race an unlocked iter) *)
      let run () =
        let r = handle_stmt tx notice stmt in
        if !tx = Idle && is_write stmt then Catalog.fsync_all ();
        r
      in
      (match with_txn_lock tx run with
       | Exec.Tag tag -> send oc (Protocol.command_complete tag)
       | Exec.Rows (desc, rows) ->
         send oc (Protocol.row_description desc);
         List.iter (fun r -> send oc (Protocol.data_row r)) rows;
         send oc (Protocol.command_complete (Printf.sprintf "SELECT %d" (List.length rows))))
    with
    | Exec.Sql_error (code, msg) -> abort_if_active tx; send oc (Protocol.error_response code msg)
    | Failure msg -> abort_if_active tx; send oc (Protocol.error_response "42601" msg)
    | e -> abort_if_active tx; send oc (Protocol.error_response "XX000" (Printexc.to_string e)));
  send oc (Protocol.ready_for_query (txn_status !tx))

let handle_connection fd =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  (* Startup: client may open with any number of SSLRequest/GSSRequest probes
     (libpq tries GSSAPI then SSL). Refuse each with 'N' until the real
     StartupMessage (protocol version 196608) arrives. *)
  let rec read_real_startup () =
    let payload = Protocol.read_startup ic in
    let code = Buf.get_int32 payload 0 in
    if code = ssl_request || code = gss_request then (
      output_char oc 'N';
      flush oc;
      read_real_startup ())
    else payload
  in
  let _startup = read_real_startup () in
  (* Handshake (trust auth: no password challenge). *)
  send oc (Protocol.authentication_ok ());
  List.iter (fun (k, v) -> send oc (Protocol.parameter_status k v)) startup_params;
  send oc (Protocol.backend_key_data 4242 2424);
  send oc (Protocol.ready_for_query 'I');
  flush oc;
  (* Per-connection session state. *)
  let tx = ref Idle in
  let prepared : (string, string) Hashtbl.t = Hashtbl.create 8 in
  (* name -> SQL *)
  let portals : (string, Sql.stmt) Hashtbl.t = Hashtbl.create 8 in
  (* name -> bound stmt *)
  let find_prepared n =
    match Hashtbl.find_opt prepared n with
    | Some q -> q
    | None -> raise (Exec.Sql_error ("26000", "prepared statement does not exist"))
  in
  let find_portal n =
    match Hashtbl.find_opt portals n with
    | Some s -> s
    | None -> raise (Exec.Sql_error ("34000", "portal does not exist"))
  in
  (* Parse: store the SQL under a statement name. *)
  let handle_parse payload =
    let sname, p = Buf.get_cstring payload 0 in
    let query, _ = Buf.get_cstring payload p in
    Hashtbl.replace prepared sname query;
    send oc (Protocol.parse_complete ())
  in
  (* Bind: read parameter values (text format assumed), substitute into the
     statement, store the bound statement as a portal. *)
  let handle_bind payload =
    let portal, p = Buf.get_cstring payload 0 in
    let sname, p = Buf.get_cstring payload p in
    let nfmt = Buf.get_int16 payload p in
    let p = ref (p + 2 + (2 * nfmt)) in
    (* skip parameter format codes *)
    let nparams = Buf.get_int16 payload !p in
    p := !p + 2;
    let params = Array.make nparams Catalog.VNull in
    for i = 0 to nparams - 1 do
      let len = Protocol.get_int32_signed payload !p in
      p := !p + 4;
      if len < 0 then params.(i) <- Catalog.VNull
      else begin
        params.(i) <- value_of_text (Some (String.sub payload !p len));
        p := !p + len
      end
    done;
    let stmt = Exec.bind params (Sql.parse (find_prepared sname)) in
    Hashtbl.replace portals portal stmt;
    send oc (Protocol.bind_complete ())
  in
  (* Describe: report parameter + result column layout. *)
  let handle_describe payload =
    let kind = payload.[0] in
    let name, _ = Buf.get_cstring payload 1 in
    let stmt, is_stmt =
      if kind = 'S' then (Sql.parse (find_prepared name), true) else (find_portal name, false)
    in
    if is_stmt then
      send oc (Protocol.parameter_description (List.init (Exec.count_params stmt) (fun _ -> 0)));
    match with_txn_lock tx (fun () -> Exec.describe stmt) with
    | Some desc -> send oc (Protocol.row_description desc)
    | None -> send oc (Protocol.no_data ())
  in
  (* Execute: run the portal. RowDescription already went out via Describe, so
     Execute emits only DataRows + CommandComplete. *)
  let handle_execute payload =
    let portal, _ = Buf.get_cstring payload 0 in
    let notice m = send oc (Protocol.notice_response m) in
    let stmt = find_portal portal in
    let run () =
      let r = handle_stmt tx notice stmt in
      if !tx = Idle && is_write stmt then Catalog.fsync_all ();
      r
    in
    (match with_txn_lock tx run with
     | Exec.Tag tag -> send oc (Protocol.command_complete tag)
     | Exec.Rows (_, rows) ->
       List.iter (fun r -> send oc (Protocol.data_row r)) rows;
       send oc (Protocol.command_complete (Printf.sprintf "SELECT %d" (List.length rows))))
  in
  let handle_close payload =
    let kind = payload.[0] in
    let name, _ = Buf.get_cstring payload 1 in
    (if kind = 'S' then Hashtbl.remove prepared name else Hashtbl.remove portals name);
    send oc (Protocol.close_complete ())
  in
  (* Message loop: simple query ('Q') and extended protocol. In extended mode an
     error skips messages until Sync ('S'), per the protocol. *)
  let continue = ref true in
  let in_error = ref false in
  while !continue do
    let tag, payload = Protocol.read_message ic in
    (try
       match tag with
       | 'Q' -> handle_query oc tx payload
       | 'P' -> if not !in_error then handle_parse payload
       | 'B' -> if not !in_error then handle_bind payload
       | 'D' -> if not !in_error then handle_describe payload
       | 'E' -> if not !in_error then handle_execute payload
       | 'C' -> if not !in_error then handle_close payload
       | 'S' ->
         in_error := false;
         send oc (Protocol.ready_for_query (txn_status !tx)) (* Sync *)
       | 'H' -> () (* Flush; buffer is flushed below *)
       | 'X' -> continue := false (* Terminate *)
       | _ -> ()
     with
     | Exec.Sql_error (code, msg) -> in_error := true; abort_if_active tx; send oc (Protocol.error_response code msg)
     | Failure msg -> in_error := true; abort_if_active tx; send oc (Protocol.error_response "42601" msg)
     | End_of_file -> continue := false
     | e -> in_error := true; abort_if_active tx; send oc (Protocol.error_response "XX000" (Printexc.to_string e)));
    if !continue then flush oc
  done

let serve () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen sock 8;
  Printf.printf "pgwire listening on 127.0.0.1:%d\n%!" port;
  while true do
    let client, _ = Unix.accept sock in
    (* one thread per connection; the catalog lock keeps their queries consistent *)
    ignore
      (Thread.create
         (fun fd ->
           (try handle_connection fd with
            | End_of_file -> ()
            | e -> Printf.eprintf "connection error: %s\n%!" (Printexc.to_string e));
           try Unix.close fd with _ -> ())
         client)
  done

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "test" then (
    Buf.demo ();
    Page.demo ();
    Sql.demo ();
    print_endline "all self-checks passed")
  else (
    Catalog.load ();
    (* restore tables persisted from a previous run *)
    serve ())
