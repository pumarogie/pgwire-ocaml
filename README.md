# pgwire-ocaml

**A small but real relational database, written from scratch in OCaml, that real `psql` connects to.**

No database libraries. No parser generators. No frameworks. Just the OCaml
standard library, `unix`, and `threads` — ~1,450 lines implementing the
PostgreSQL wire protocol, a SQL parser, a query planner, on-disk storage, and
concurrency. Point the real `psql` client at it and run queries against your own
engine.

```console
$ psql -h 127.0.0.1 -p 5434 -U postgres
psql (14.15)
Type "help" for help.

postgres=> CREATE TABLE users (id int, name text);
CREATE TABLE
postgres=> INSERT INTO users VALUES (1, 'alice');
INSERT 0 1
postgres=> CREATE INDEX ON users (id);
CREATE INDEX
postgres=> SELECT name FROM users WHERE id = 1;
NOTICE:  index scan on users.id
 name
-------
 alice
(1 row)
```

That `NOTICE` is the query planner telling you which access path it chose — one
of several ways this project makes database internals *visible* rather than
theoretical.

---

## Why this exists

To learn how a database actually works by building one, end to end. Every module
maps to a real database concept, and each was built as an independent, testable
milestone:

| Layer | Real-world concept | File |
|-------|-------------------|------|
| Wire protocol v3 | Client/server boundary — how `psql`, JDBC, psycopg talk to Postgres | `lib/protocol.ml`, `lib/buf.ml` |
| SQL parser | Lexer + recursive-descent parser (compilers 101, applied) | `lib/sql.ml` |
| Executor + planner | Access-path selection, aggregation, sorting | `lib/exec.ml` |
| Storage engine | Slotted heap pages on disk — how Postgres lays out tuples | `lib/catalog.ml`, `lib/page.ml` |
| Indexes | Ordered secondary structures for point + range lookup | `lib/catalog.ml` |
| Concurrency | Thread-per-connection + lock-based serialization | `bin/main.ml` |

OCaml is the natural language for this: algebraic data types and pattern
matching make the parser and executor terse and correct, and the wire protocol
is pure byte manipulation the type system keeps honest.

---

## Features

- **Real PostgreSQL wire protocol v3** — genuine `psql` / libpq clients connect
  over TCP. Startup handshake, SSL/GSS negotiation refusal, trust auth,
  `ParameterStatus`, `ReadyForQuery`.
- **Simple query protocol** (`Q`) — one-shot SQL.
- **Extended query protocol** (`Parse`/`Bind`/`Describe`/`Execute`/`Sync`) —
  prepared statements and `$1` bind parameters, exactly what production drivers
  use. No string-splicing, no SQL injection.
- **SQL subset**
  - `CREATE TABLE t (a int, b text, c float, d bool)` — types: `int`, `text`, `float`, `bool`
  - `CREATE INDEX [name] ON t (col)`
  - `INSERT INTO t VALUES (...)` — literals or `$n` params
  - `UPDATE t SET col = val [, ...] [WHERE col <op> val]`
  - `DELETE FROM t [WHERE col <op> val]`
  - `SELECT [DISTINCT] <items> FROM t [WHERE pred] [GROUP BY col] [HAVING agg <op> val] [ORDER BY col [ASC|DESC]] [LIMIT n]`
    - items: `*`, columns, or aggregates `COUNT(*) COUNT(c) SUM(c) MIN(c) MAX(c) AVG(c)`
      (SUM/MIN/MAX keep the column type; AVG is always fractional)
    - predicates: `col <op> val` with `=  <  <=  >  >=`, or `col IS [NOT] NULL`,
      combined with `AND` / `OR` and parentheses (AND binds tighter)
    - proper three-valued logic: a comparison with a NULL operand is never true
  - `SELECT <items> FROM a [INNER|LEFT|RIGHT|FULL] JOIN b ON a.col = b.col [WHERE ...] [ORDER BY ...] [LIMIT n]`
    — equi-join, all four kinds; qualified (`a.col`) or unqualified column references;
      outer joins NULL-pad the unmatched side(s)
  - `BEGIN` / `COMMIT` / `ROLLBACK` — transactions with snapshot-based rollback
- **Query planner** — **seq scan**, **index scan** (equality), or **index range
  scan** for table access; a range predicate falls back to a seq scan when it's
  not selective enough (estimated from the index's min/max, like PG's range
  estimate). For joins: **hash join** (O(n+m), large inputs) vs **nested-loop**
  (small inputs), chosen by actual row counts. Every choice is reported as a
  `NOTICE`.
- **On-disk storage** — each table is a heap file of one or more slotted 8 KB
  pages (rows overflow to new pages as needed) plus a schema file. Data survives
  restarts.
- **Ordered indexes** — balanced-tree (stdlib `Map`) secondary index serving
  both equality and range queries, maintained incrementally on every insert.
- **Transactions** — `BEGIN` / `COMMIT` / `ROLLBACK`. `BEGIN` snapshots the
  catalog, `ROLLBACK` restores it (undoing inserts, updates, deletes, and even
  tables created mid-transaction), `COMMIT` discards the snapshot. The
  transaction status (`T`/`I`/`E`) is reported back in every `ReadyForQuery`; an
  error inside a transaction aborts it until `ROLLBACK`.
- **Durability** — the WAL is `fsync`'d at the commit boundary: an autocommit
  write is fsynced when it completes; inside a transaction the fsync happens once
  at `COMMIT` (group commit). Batch bulk loads in a `BEGIN`/`COMMIT` for a single
  fsync — the same trade-off PostgreSQL makes.
- **Concurrency** — thread per connection; a global lock serializes catalog
  access so writes stay consistent.
- **Proper errors** — `ErrorResponse` with real SQLSTATE codes
  (`42P01` undefined table, `42703` undefined column, `42601` syntax, …).

---

## Architecture

```
                        TCP :5434
   psql / libpq  ───────────────────────►  bin/main.ml
   (real client)                            accept loop, thread per conn,
                                            startup handshake, message loop,
                                            global catalog lock
                                                   │
              ┌────────────────────────────────────┼───────────────────────────┐
              ▼                                     ▼                            ▼
        lib/protocol.ml                        lib/sql.ml                   lib/exec.ml
        encode/decode wire msgs            lexer + parser → AST         planner + executor
        (built on lib/buf.ml)                                          (aggregate/order/limit,
                                                                        access-path choice)
                                                                             │
                                                                             ▼
                                                                       lib/catalog.ml
                                                                   tables, rows, ordered
                                                                   indexes, persistence
                                                                             │
                                                                             ▼
                                                                        lib/page.ml
                                                                   slotted 8 KB heap page
```

A query's life: bytes → `Protocol.read_message` → `Sql.parse` → `Exec.run`
(planner picks an access path against `Catalog`, which reads/writes `Page`s on
disk) → `Protocol` encodes `RowDescription`/`DataRow`/`CommandComplete` → bytes.

---

## Quick start

Requires OCaml + `dune` + `opam` (and `psql` to play client).

```bash
# build
dune build

# run the tests (15 assert-based cases across every layer)
dune runtest

# start the server (listens on 127.0.0.1:5434; override with PGWIRE_PORT)
dune exec bin/main.exe

# in another terminal
psql -h 127.0.0.1 -p 5434 -U postgres
```

Any username works (trust auth). Data is written under `./pgdata` by default;
set `PGWIRE_DATA` to relocate it.

Try it:

```sql
CREATE TABLE games (id int, team text, score int);
INSERT INTO games VALUES (1, 'red', 10);
INSERT INTO games VALUES (2, 'red', 30);
INSERT INTO games VALUES (3, 'blue', 20);

SELECT team, COUNT(*), SUM(score) FROM games GROUP BY team;
SELECT id, score FROM games ORDER BY score DESC LIMIT 2;

CREATE INDEX ON games (score);
SELECT id FROM games WHERE score > 15;      -- NOTICE: index range scan on games.score
```

---

## Benchmarks

```bash
dune exec --profile release bench/bench.exe
```

`bench/bench.ml` times every query category at several data scales and prints
the numbers. Highlights (Apple silicon, in-memory + local disk):

| Operation | Scale | Time | Complexity |
|-----------|-------|------|-----------|
| INSERT (per row) | any | **~2.7 µs, flat** | O(1) — WAL append (~16 bytes, sequential) |
| `ORDER BY` indexed col `+ LIMIT` | 100k | **0.004 ms** | top-k — walks the index, touches only n rows |
| index point scan | 100k | 0.004 ms | O(log n) |
| seq scan (point) | 100k | 0.8 ms | O(n), compiled predicate |
| aggregate COUNT+SUM+AVG | 100k | 2.6 ms | O(n), single pass for all aggregates |
| compound WHERE (AND/OR) | 100k | 4.2 ms | O(n), compiled predicate tree |
| DISTINCT | 100k | 4.7 ms | O(n), dedupe on values before rendering |
| GROUP BY | 100k → 10 groups | 3.4 ms | O(n), fused hash aggregation |
| seq range (50k rows) | 100k | 17 ms | O(n) + rendering 50k rows |
| hash join | 50k × 50k | 39 ms | O(n + m), value join keys |

Optimizations applied (all test-covered, behavior unchanged):
- **Rows are `value array`** — O(1) column access (was `List.nth`, O(i)); sped up
  every column-touching op 1.2–15×.
- **Write-ahead log** — INSERT appends the ~16-byte tuple sequentially instead of
  rewriting an 8 KB page (67 → 2.7 µs/row, ~25×; pages are checkpoints replayed
  on load). Now in PostgreSQL `COPY` territory.
- **Index-ordered scan + LIMIT pushdown** — `ORDER BY` on an indexed column skips
  the sort; `ORDER BY … LIMIT n` becomes a top-k touching only n rows
  (217 ms → 0.004 ms).
- **Memoized forward row list** — the per-query `List.rev` is cached and reused
  until the next write (seq scan 19 → 3.6 ms).
- **Compiled predicates** — a WHERE clause is compiled once into a `row -> bool`
  with column indices and literals baked in, instead of re-resolving column names
  per row (seq scan 3.6 → 1.4 ms, compound WHERE 8.6 → 3.5 ms).
- **Allocation-free keys** — GROUP BY and hash join key on the value itself, and
  DISTINCT dedupes on selected values before rendering, dropping the per-row
  string allocation (DISTINCT 31 → 4.7 ms, hash join 90 → 39 ms).
- **Tail-recursive result handling** — the executor builds row lists with
  tail-recursive `map`/`filter`/`concat_map`/`append`, so a query returning many
  rows can't overflow the connection thread's small (~512 KB) stack. (The stdlib
  versions are non-tail-recursive in OCaml 4.12 and crashed large results.)
- **Fused hash aggregation** — a single pass builds one accumulator set per group
  (like PostgreSQL's HashAggregate), instead of materializing per-group row lists
  and re-scanning each group per aggregate function (aggregate 56 → 2.7 ms,
  GROUP BY 53 → 3.4 ms).

## Testing

```bash
dune runtest
```

15 assert-based tests (`test/test_pgwire.ml`) cover every layer — they fail the
build on any regression:

- **storage** — slotted-page round-trip, tuple encode/decode (incl. NULL and
  negative ints), persistence reload after a simulated restart
- **parser** — every statement form, params, aggregates, order/limit
- **planner** — seq scan vs index scan vs index **range** scan selection
- **executor** — projection, WHERE, aggregates, GROUP BY, ORDER BY, LIMIT
- **params** — bind substitution and parameter counting
- **errors** — correct SQLSTATE codes raised

New features are developed **test-first**: write the failing assertions, watch
them fail to compile/run, then implement to green.

The extended (prepared-statement) protocol is additionally exercised end-to-end
by a raw-socket client — see the walkthrough in the commit history.

Every non-trivial module also carries an `assert`-based self-check, runnable
directly:

```bash
dune exec bin/main.exe test      # buf / page / sql self-checks
```

---

## Project layout

```
pgwire-ocaml/
├── bin/
│   └── main.ml         TCP server, connection threads, protocol message loop   (223)
├── lib/
│   ├── buf.ml          big-endian read/write + message framing                  (65)
│   ├── protocol.ml     encode backend msgs / decode frontend msgs              (129)
│   ├── sql.ml          lexer + recursive-descent parser → AST                  (293)
│   ├── catalog.ml      tables, rows, ordered indexes, on-disk persistence      (229)
│   ├── exec.ml         query planner + executor                                (229)
│   └── page.ml         slotted 8 KB heap page                                   (85)
├── test/
│   └── test_pgwire.ml  assert-based test suite
└── bench/
    └── bench.ml        microbenchmarks for every query category at scale
```

---

## Database internals, made visible

Things you can *see* this project doing, that are usually hidden:

- **Access paths.** Every `SELECT` emits a `NOTICE` naming the plan the executor
  chose: `seq scan`, `index scan`, or `index range scan`. Add an index and watch
  the plan change with no change to the query.
- **The wire is just bytes.** `lib/buf.ml` and `lib/protocol.ml` build every
  message by hand — you can read exactly what goes over the socket.
- **Tuples live in pages.** `lib/page.ml` implements the slotted-page layout
  Postgres uses: a slot array growing forward from the header, tuple data growing
  backward from the end, free space in the middle. Row identity survives tuples
  moving within a page.
- **Prepared statements are a protocol, not a syntax.** `PREPARE` in SQL is not
  the same thing as the Bind/Execute message flow drivers actually use — this
  implements the latter.

---

## Deliberate simplifications (and the upgrade path)

This is a teaching engine. Shortcuts are intentional and marked in the code with
`ponytail:` comments naming the ceiling and how a real engine lifts it:

| Shortcut | Ceiling | Real-database upgrade |
|----------|---------|----------------------|
| WAL uses plain `fsync`, not macOS `F_FULLFSYNC` | not fully power-loss durable on macOS | `fcntl F_FULLFSYNC` for true durability |
| Row must fit in one 8 KB page | No huge values | TOAST (out-of-line storage for oversized attributes) |
| Joins: hash (large) or nested-loop (small); no merge join | No sorted-input fast path | Cost-based merge join |
| A transaction holds the global lock start-to-end | Other connections block for the whole txn (serial, but correct) | Per-row locks + MVCC |
| Snapshot-based transactions (whole catalog) | Large snapshot cost on big catalogs | MVCC + write-ahead log |
| Index range scan is an O(n) fold | No better than seq scan on huge tables | On-disk B-tree leaf walk |
| In-memory indexes | Re-`CREATE` after restart | Persisted on-disk B-trees |
| Float SUM may lose precision on huge values | Rounding on large sums | Kahan summation / exact numeric |
| Text-typed bind params | Types inferred by looks | Honor parameter type OIDs |
| Single statement per query | No multi-statement `Q` | Statement splitting |
| Trust auth only, SSL refused | Localhost only | SCRAM auth + TLS |

A write-ahead log, on-disk B-trees, and table aliases (`FROM emp e`) are natural
next milestones.

---

## Requirements

- OCaml (developed on 4.12) + `dune` ≥ 3.0
- `unix` and `threads.posix` (both ship with OCaml)
- `psql` / PostgreSQL client to connect (optional; any libpq-based driver works)

Built as a learning project. Not intended for production use.
