(* sql.ml — hand-written lexer + recursive-descent parser for a tiny SQL subset.

   Supported:
     CREATE TABLE t (a int, b text)
     CREATE INDEX [name] ON t (col)
     INSERT INTO t VALUES (1, 'x')          -- values may be $n bind params
     SELECT <items> FROM t
         [WHERE col = value] [GROUP BY col] [ORDER BY col [ASC|DESC]] [LIMIT n]
       where items are: star | col | AGG of col | COUNT star; AGG in count/sum/min/max/avg
   Anything else (SET, BEGIN, ...) parses to [Other tag].

   Unquoted identifiers are folded to lowercase, matching PostgreSQL. *)

(* a value position is either a literal or a $n bind parameter (extended protocol) *)
type pvalue = Lit of Catalog.value | Param of int

type agg = Count | Sum | Min | Max | Avg
type sel_item = Star | Col of string | Agg of agg * string option (* arg None = star form *)
type order = { by : string; desc : bool }

(* FROM clause: a single table, or an equi-join of two tables.
   Column references may be qualified ("emp.dept") — kept as "tbl.col" strings. *)
type join_kind = Inner | Left | Right | Full
type source = Table of string | Join of string * string * (string * string) * join_kind

(* a WHERE predicate: comparison / IS [NOT] NULL / boolean combinations *)
type pred =
  | Cmp of string * Catalog.cmp * pvalue
  | Null of string * bool (* true = IS NOT NULL *)
  | And of pred * pred
  | Or of pred * pred

type cond = pred option (* optional WHERE clause *)

type stmt =
  | Create of string * (string * Catalog.typ) list
  | CreateIndex of string * string (* table, column *)
  | Insert of string * pvalue list
  | Select of select
  | Delete of string * cond
  | Update of string * (string * pvalue) list * cond (* table, SET assignments, WHERE *)
  | Other of string (* CommandComplete tag to report, e.g. "SET" *)

and select = {
  distinct : bool;
  items : sel_item list;
  from : source;
  where : cond;
  group_by : string option;
  having : (sel_item * Catalog.cmp * pvalue) option; (* filter groups by an aggregate/column *)
  order_by : order option;
  limit : int option;
}

(* --- lexer --- *)

type tok =
  | TIdent of string
  | TNum of int
  | TStr of string
  | TSym of char
  | TParam of int
  | TFloat of float
  | TCmp of Catalog.cmp (* < <= > >= *)

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')

let tokenize s =
  let n = String.length s in
  let toks = ref [] in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = '-' && !i + 1 < n && s.[!i + 1] = '-' then
      (* line comment: skip to end of line *)
      while !i < n && s.[!i] <> '\n' do incr i done
    else if c = '/' && !i + 1 < n && s.[!i + 1] = '*' then begin
      (* block comment: skip to the closing */ *)
      i := !i + 2;
      while !i + 1 < n && not (s.[!i] = '*' && s.[!i + 1] = '/') do incr i done;
      i := if !i + 1 < n then !i + 2 else n
    end
    else if c = '(' || c = ')' || c = ',' || c = ';' || c = '=' || c = '*' || c = '.' then (
      toks := TSym c :: !toks;
      incr i)
    else if c = '<' then
      if !i + 1 < n && s.[!i + 1] = '=' then (toks := TCmp Catalog.Le :: !toks; i := !i + 2)
      else (toks := TCmp Catalog.Lt :: !toks; incr i)
    else if c = '>' then
      if !i + 1 < n && s.[!i + 1] = '=' then (toks := TCmp Catalog.Ge :: !toks; i := !i + 2)
      else (toks := TCmp Catalog.Gt :: !toks; incr i)
    else if c = '\'' then begin
      (* single-quoted string; '' is an escaped quote *)
      incr i;
      let b = Buffer.create 16 in
      let fin = ref false in
      while not !fin do
        if !i >= n then failwith "unterminated string literal";
        let d = s.[!i] in
        if d = '\'' then
          if !i + 1 < n && s.[!i + 1] = '\'' then (
            Buffer.add_char b '\'';
            i := !i + 2)
          else (
            incr i;
            fin := true)
        else (
          Buffer.add_char b d;
          incr i)
      done;
      toks := TStr (Buffer.contents b) :: !toks
    end
    else if c = '$' then begin
      (* $n bind parameter placeholder *)
      let j = ref (!i + 1) in
      while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
      if !j = !i + 1 then failwith "expected digits after '$'";
      toks := TParam (int_of_string (String.sub s (!i + 1) (!j - !i - 1))) :: !toks;
      i := !j
    end
    else if (c >= '0' && c <= '9') || (c = '-' && !i + 1 < n && s.[!i + 1] >= '0' && s.[!i + 1] <= '9') then begin
      (* optional leading '-' (there is no subtraction operator, so '-' before a
         digit is always a sign), then digits, optionally a fractional part *)
      let start = !i in
      let j = ref (if c = '-' then !i + 1 else !i) in
      while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
      (* a '.' followed by a digit makes it a float (vs. the '.' in a.col) *)
      if !j + 1 < n && s.[!j] = '.' && s.[!j + 1] >= '0' && s.[!j + 1] <= '9' then begin
        incr j;
        while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
        toks := TFloat (float_of_string (String.sub s start (!j - start))) :: !toks
      end
      else toks := TNum (int_of_string (String.sub s start (!j - start))) :: !toks;
      i := !j
    end
    else if is_ident_start c then begin
      let j = ref !i in
      while !j < n && is_ident_char s.[!j] do incr j done;
      toks := TIdent (String.sub s !i (!j - !i)) :: !toks;
      i := !j
    end
    else failwith (Printf.sprintf "unexpected character '%c'" c)
  done;
  Array.of_list (List.rev !toks)

(* --- parser --- *)

let agg_of = function
  | "count" -> Count
  | "sum" -> Sum
  | "min" -> Min
  | "max" -> Max
  | "avg" -> Avg
  | _ -> assert false

let is_agg = function "count" | "sum" | "min" | "max" | "avg" -> true | _ -> false

let parse sql =
  let toks = tokenize sql in
  let pos = ref 0 in
  let peek () = if !pos < Array.length toks then Some toks.(!pos) else None in
  let peek2 () = if !pos + 1 < Array.length toks then Some toks.(!pos + 1) else None in
  let advance () = incr pos in
  let sym c =
    match peek () with
    | Some (TSym x) when x = c -> advance ()
    | _ -> failwith (Printf.sprintf "expected '%c'" c)
  in
  let name () =
    match peek () with
    | Some (TIdent s) -> advance (); String.lowercase_ascii s
    | _ -> failwith "expected identifier"
  in
  (* a possibly-qualified column: "col" or "tbl.col" (kept as a "tbl.col" string) *)
  let colname () =
    let a = name () in
    match peek () with Some (TSym '.') -> advance (); a ^ "." ^ name () | _ -> a
  in
  let kw () =
    match peek () with
    | Some (TIdent s) -> advance (); String.lowercase_ascii s
    | _ -> failwith "expected keyword"
  in
  let expect_kw w = if kw () <> w then failwith (Printf.sprintf "expected %s" (String.uppercase_ascii w)) in
  (* consume keyword w if it is next; return whether it was there *)
  let opt_kw w =
    match peek () with
    | Some (TIdent x) when String.lowercase_ascii x = w -> advance (); true
    | _ -> false
  in
  let value () =
    match peek () with
    | Some (TNum n) -> advance (); Lit (Catalog.VInt n)
    | Some (TFloat f) -> advance (); Lit (Catalog.VFloat f)
    | Some (TStr s) -> advance (); Lit (Catalog.VText s)
    | Some (TParam k) -> advance (); Param k
    | Some (TIdent w) -> (
      advance ();
      match String.lowercase_ascii w with
      | "true" -> Lit (Catalog.VBool true)
      | "false" -> Lit (Catalog.VBool false)
      | "null" -> Lit Catalog.VNull
      | o -> failwith (Printf.sprintf "unexpected identifier %s in value" o))
    | _ -> failwith "expected a value"
  in
  let typ () =
    match kw () with
    | "int" | "integer" | "int4" -> Catalog.Int
    | "text" | "varchar" | "string" -> Catalog.Text
    | "bool" | "boolean" -> Catalog.Bool
    | "float" | "real" -> Catalog.Float
    | "double" -> ignore (opt_kw "precision"); Catalog.Float
    | t -> failwith (Printf.sprintf "unknown type %s" t)
  in
  let paren_list item =
    sym '(';
    let acc = ref [ item () ] in
    let rec loop () =
      match peek () with
      | Some (TSym ',') -> advance (); acc := item () :: !acc; loop ()
      | _ -> ()
    in
    loop ();
    sym ')';
    List.rev !acc
  in
  (* predicate grammar: OR of ANDs of atoms; atom = comparison | IS NULL | ( expr ) *)
  let rec parse_or () =
    let l = parse_and () in
    if opt_kw "or" then Or (l, parse_or ()) else l
  and parse_and () =
    let l = parse_atom () in
    if opt_kw "and" then And (l, parse_and ()) else l
  and parse_atom () =
    match peek () with
    | Some (TSym '(') -> advance (); let p = parse_or () in sym ')'; p
    | _ ->
      let c = colname () in
      (match peek () with
       | Some (TIdent w) when String.lowercase_ascii w = "is" ->
         advance (); let neg = opt_kw "not" in expect_kw "null"; Null (c, neg)
       | Some (TSym '=') -> advance (); Cmp (c, Catalog.Eq, value ())
       | Some (TCmp o) -> advance (); Cmp (c, o, value ())
       | _ -> failwith "expected a comparison operator or IS [NOT] NULL")
  in
  let parse_where () = if opt_kw "where" then Some (parse_or ()) else None in
  let parse_create () =
    expect_kw "create";
    match kw () with
    | "table" ->
      let t = name () in
      let cols = paren_list (fun () -> let c = name () in let ty = typ () in (c, ty)) in
      Create (t, cols)
    | "index" ->
      (match peek () with
       | Some (TIdent w) when String.lowercase_ascii w = "on" -> advance ()
       | _ -> let _ = name () in expect_kw "on");
      let t = name () in
      sym '(';
      let c = name () in
      sym ')';
      CreateIndex (t, c)
    | other -> failwith (Printf.sprintf "expected TABLE or INDEX, got %s" other)
  in
  let parse_insert () =
    expect_kw "insert";
    expect_kw "into";
    let t = name () in
    expect_kw "values";
    let vals = paren_list value in
    Insert (t, vals)
  in
  let sel_item () =
    match peek () with
    | Some (TSym '*') -> advance (); Star
    | Some (TIdent w) when is_agg (String.lowercase_ascii w) && peek2 () = Some (TSym '(') ->
      let a = agg_of (String.lowercase_ascii w) in
      advance ();
      sym '(';
      let arg = match peek () with Some (TSym '*') -> advance (); None | _ -> Some (colname ()) in
      sym ')';
      Agg (a, arg)
    | Some (TIdent _) -> Col (colname ())
    | _ -> failwith "expected column, '*', or aggregate"
  in
  let parse_select () =
    expect_kw "select";
    let distinct = opt_kw "distinct" in
    let items = ref [ sel_item () ] in
    let rec more () =
      match peek () with
      | Some (TSym ',') -> advance (); items := sel_item () :: !items; more ()
      | _ -> ()
    in
    more ();
    expect_kw "from";
    let t1 = name () in
    let build kind =
      let t2 = name () in
      expect_kw "on";
      let l = colname () in
      (match peek () with Some (TSym '=') -> advance () | _ -> failwith "expected '=' in JOIN ON");
      let r = colname () in
      Join (t1, t2, (l, r), kind)
    in
    let from =
      if opt_kw "left" then (ignore (opt_kw "outer"); expect_kw "join"; build Left)
      else if opt_kw "right" then (ignore (opt_kw "outer"); expect_kw "join"; build Right)
      else if opt_kw "full" then (ignore (opt_kw "outer"); expect_kw "join"; build Full)
      else if opt_kw "join" then build Inner
      else Table t1
    in
    let where = parse_where () in
    let group_by = if opt_kw "group" then (expect_kw "by"; Some (name ())) else None in
    let having =
      if opt_kw "having" then (
        let item = sel_item () in
        let op =
          match peek () with
          | Some (TSym '=') -> advance (); Catalog.Eq
          | Some (TCmp o) -> advance (); o
          | _ -> failwith "expected a comparison operator in HAVING"
        in
        Some (item, op, value ()))
      else None
    in
    let order_by =
      if opt_kw "order" then (
        expect_kw "by";
        let by = colname () in
        let desc = if opt_kw "desc" then true else (ignore (opt_kw "asc"); false) in
        Some { by; desc })
      else None
    in
    let limit =
      if opt_kw "limit" then
        match peek () with
        | Some (TNum n) -> advance (); Some n
        | _ -> failwith "expected a number after LIMIT"
      else None
    in
    Select { distinct; items = List.rev !items; from; where; group_by; having; order_by; limit }
  in
  let parse_delete () =
    expect_kw "delete";
    expect_kw "from";
    let t = name () in
    Delete (t, parse_where ())
  in
  let parse_update () =
    expect_kw "update";
    let t = name () in
    expect_kw "set";
    let assign () = let c = name () in sym '='; (c, value ()) in
    let acc = ref [ assign () ] in
    let rec loop () =
      match peek () with
      | Some (TSym ',') -> advance (); acc := assign () :: !acc; loop ()
      | _ -> ()
    in
    loop ();
    Update (t, List.rev !acc, parse_where ())
  in
  match peek () with
  | Some (TIdent w) -> (
    match String.lowercase_ascii w with
    | "create" -> parse_create ()
    | "insert" -> parse_insert ()
    | "select" -> parse_select ()
    | "delete" -> parse_delete ()
    | "update" -> parse_update ()
    | other -> Other (String.uppercase_ascii other))
  | _ -> failwith "empty or invalid statement"

(* self-check: run via `main test` *)
let demo () =
  (match parse "CREATE TABLE t (a int, b text)" with
   | Create ("t", [ ("a", Catalog.Int); ("b", Catalog.Text) ]) -> ()
   | _ -> assert false);
  (match parse "INSERT INTO t VALUES (1, 'x')" with
   | Insert ("t", [ Lit (Catalog.VInt 1); Lit (Catalog.VText "x") ]) -> ()
   | _ -> assert false);
  (match parse "SELECT * FROM t" with
   | Select { items = [ Star ]; from = Table "t"; where = None; _ } -> ()
   | _ -> assert false);
  (match parse "SELECT a, b FROM t WHERE a = 1" with
   | Select { items = [ Col "a"; Col "b" ]; where = Some (Cmp ("a", Catalog.Eq, Lit (Catalog.VInt 1))); _ } -> ()
   | _ -> assert false);
  (match parse "SELECT a FROM t WHERE a IS NOT NULL" with
   | Select { where = Some (Null ("a", true)); _ } -> ()
   | _ -> assert false);
  (match parse "SELECT a.x FROM a JOIN b ON a.k = b.k" with
   | Select { items = [ Col "a.x" ]; from = Join ("a", "b", ("a.k", "b.k"), Inner); _ } -> ()
   | _ -> assert false);
  (match parse "SELECT a.x FROM a LEFT JOIN b ON a.k = b.k" with
   | Select { from = Join ("a", "b", ("a.k", "b.k"), Left); _ } -> ()
   | _ -> assert false);
  (match parse "SELECT a FROM t WHERE a >= 5" with
   | Select { where = Some (Cmp ("a", Catalog.Ge, Lit (Catalog.VInt 5))); _ } -> ()
   | _ -> assert false);
  (match parse "INSERT INTO t VALUES ($1, $2)" with
   | Insert ("t", [ Param 1; Param 2 ]) -> ()
   | _ -> assert false);
  (match parse "CREATE INDEX ON t (a)" with CreateIndex ("t", "a") -> () | _ -> assert false);
  (match parse "DELETE FROM t WHERE a = 1" with
   | Delete ("t", Some (Cmp ("a", Catalog.Eq, Lit (Catalog.VInt 1)))) -> ()
   | _ -> assert false);
  (match parse "UPDATE t SET a = 2, b = 'z' WHERE a = 1" with
   | Update ("t", [ ("a", Lit (Catalog.VInt 2)); ("b", Lit (Catalog.VText "z")) ], Some (Cmp ("a", Catalog.Eq, _))) -> ()
   | _ -> assert false);
  (match parse "SELECT COUNT(*), SUM(a) FROM t GROUP BY b ORDER BY a DESC LIMIT 3" with
   | Select { items = [ Agg (Count, None); Agg (Sum, Some "a") ]; group_by = Some "b";
              order_by = Some { by = "a"; desc = true }; limit = Some 3; _ } -> ()
   | _ -> assert false);
  print_endline "sql: ok"
