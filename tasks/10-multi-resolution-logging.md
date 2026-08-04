# Multi-Resolution Logging for estimate_polyomino

## Problem
Long-running stochastic estimation runs (potentially years) need a way to track progress evolution over extended periods. The current sysout table is fine for short-term monitoring but doesn't help review how estimates evolved over hours, days, or weeks.

## Proposed Solution: Hierarchical Downsampled Log Pipeline

A series of log files (L0, L1, L2, ...) where each level provides a compressed view of the previous level. Each file stays bounded in size while preserving recent detail and long-term history.

### Compression Strategy (v1)
- Simple line dropping — no aggregation yet (min/max/avg for v2)
- Keep entries at indices 0, 10, 20, 30... (every 10th) from the oldest half

### Sliding Window Design
Instead of compressing the entire log, only compress the **oldest half**:

```
L0: [oldest 50 → compress to ~5] | newest 50 (hot, full resolution)
         ↓                              ↑
L1: [oldest 50 → compress to ~5] | newest 50 (hot)
         ↓                              ↑
L2: [oldest 50 → compress to ~5] | newest 50 (hot)
```

- Each level holds **100 entries max** (50 oldest + 50 newest)
- When L0 overflows, compress its oldest 50 entries (keep every 10th = ~5) and append to L1
- L1's newest 50 stay intact; its oldest 50 get compressed to ~5 and appended to L2
- This repeats up the chain

### Coverage (at 1 batch/sec, ratio 1:10)

| Level | Hot data | Old compressed | Total span covered |
|-------|----------|----------------|-------------------|
| L0 | 50 batches (1:1) | ~250 batches | ~300 batches (~5 min) |
| L1 | 50 entries from L0 = ~250 batches | ~1,250 batches | ~1,500 batches (~25 min) |
| L2 | 50 entries from L1 = ~2,500 batches | ~12,500 batches | ~15,000 batches (~4 hours) |
| L3 | 50 entries from L2 = ~12,500 batches | ~62,500 batches | ~75,000 batches (~21 hours) |
| L4 | 50 entries from L3 = ~62,500 batches | ~312,500 batches | ~375,000 batches (~4.3 days) |

Add more levels as needed for longer runs.

### File Naming
- Format: `logs-<stamp>-L<num>.csv`
- `<stamp>` matches the session start timestamp (same as used for `solutions-<stamp>.txt`)
- `<num>` is the level (0, 1, 2, ...)
- Example: `logs-2025-06-15-14-30-L0.csv`, `logs-2025-06-15-14-30-L1.csv`
- Logs for each session stay separate/distinct

### CSV Format
- Drop `%Under` column (noisy at low resolution)
- Keep all other columns:
  `Batch, Samples, Nodes Est, Fails Est, Sols Est, Found, Materialized, Pruned, Net Nodes, %Done, Elapsed, ETA`
- Use `Printf.sprintf` with maximal precision (e.g., `%.17g` for floats, `%d` for ints)
- Not optimized for human viewing — this is a data storage format

## Implementation Plan

### Module: `searchspace/multi_log.ml` (generic log-cycling library)

Completely agnostic about line content — treats lines as opaque strings. The cycling mechanic doesn't care what's in them.

**API:**
```ocaml
type t

val create : stamp:string -> header:string -> t
(** Create a cycling log pipeline. All levels share the same CSV header.
    Levels are created lazily — no num_levels parameter. *)

val add_line : t -> string -> unit
(** Add a line to L0. Triggers cycling/compression if needed.
    Autoflushes all levels to disk after each add. *)

val close : t -> unit
(** Final flush and cleanup. Optional — add_line already autoflushes for crash safety. *)
```

**Caller code is trivial:**
```ocaml
let pipeline = Multi_log.create ~stamp ~header in
(* In progress callback — just format and add *)
Multi_log.add_line pipeline (Printf.sprintf "%d,%d,%.17g,..." batch samples ...);
(* On shutdown — optional since add_line already autoflushes *)
Multi_log.close pipeline
```

**Internally manages:**
- Each level as a list of strings (max 100 entries, 50+50 sliding window)
- Compression logic (oldest 50 → keep every 10th → ~5 entries cascade to next level)
- File I/O on flush (autoflush after every `add_line` — batch processing is heavy enough that extra I/O is negligible)
- Lazy level creation (new levels appear when data first cascades into them)

### Step 1: `let%expect_test` — smoke test with 1,000,000 lines

Write a diagnostic script as a `let%expect_test` with an empty `[%expect {| |}]`. Run it, inspect the output, then `dune promote` to enshrine.

```ocaml
let print_file_summary fname =
  let ch = In_channel.create fname in
  let lines = ref [] in
  try
    while true do
      lines := input_line ch :: !lines
    done
  with End_of_file -> close_in ch;
  let lines = List.rev !lines in
  Printf.printf "%s:\n---\n" fname;
  List.iter (fun l -> Printf.printf "%s\n" l) lines; (* or: first 3, ..., last 3 *)
  Printf.printf "---\n";

let%expect_test "multi_log 1M lines" =
  let stamp = "2025-06-15-14-30" in
  let header = "level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta" in
  let pipeline = Multi_log.create ~stamp ~header in
  
  (* Shove 1_000_000 lines into L0 *)
  for i = 1 to 1_000_000 do
    Multi_log.add_line pipeline (Printf.sprintf "line-%d" i)
  done;
  
  Multi_log.close pipeline;
  
  (* Print summaries of all log files *)
  let files = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with f ".csv"
  ) in
  List.iter print_file_summary (List.sort String.compare files);
  [%expect {| |}]
```

Workflow: run `dune test` → examine output → verify it looks right → `dune promote`

Things to check in the output:
- File naming: `logs-2025-06-15-14-30-L0.csv`, `logs-2025-06-15-14-30-L1.csv`, etc.
- Each file starts with the header line, then data lines
- L0 has ~50 lines (newest half of 100, oldest compressed out)
- L1 has ~95 lines (5 from L0 compression + 50 hot + some old)
- L2 has ~5 lines (from L1 compression)
- L3+ may or may not exist depending on cascade depth
- Lines are in chronological order (oldest first)

### Integration into `estimate_polyomino.ml`

1. Create the pipeline early in main (after parsing args, same stamp as solutions file)
2. Call `add_line` from the `on_progress` callback after table logging
3. Call `close` on shutdown (Ctrl-C or normal completion)
4. CLI option to enable logging — e.g., `--log` flag (off by default)
   - Logs go next to the puzzle file, no separate `logs/` subdirectory

## Remaining Questions

1. **CSV format** — settled: drop `%Under`, use `Printf.sprintf` with maximal precision (e.g., `%.17g` for floats, `%d` for ints). Not optimized for human viewing.

2. **File naming** — settled on `logs-<stamp>-L<num>.csv` where `<stamp>` matches the session start (same as solutions file).

3. **When to start logging** — batch 0 from the start. For fresh runs it has no real data, but for resumed runs (loaded state) batch 0 contains the pre-existing data.

4. **Compression ratio** — fixed at 1:10 (keep every 10th). Not configurable — the numbers only work well because of their divisibility relationship (100 overflow, 50 half, 10 ratio).

5. **Batch size per level** — fixed at 100 entries (50+50). Same as Q4: the numbers work together and shouldn't be configurable.

6. **Cleanup** — no explicit purging needed. Old data is implicitly "purged" during compression: when cycling entries to the next level, ~90% are dropped. The pipeline is self-limiting by design.



7. **v2 features** — not right now. Leave aggregated stats (min/max/avg) for a separate addition.

8. **Monitoring script** — not right now. No plotting tool for this iteration.
