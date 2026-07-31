(** Runtime events-based memory measurement.

Provides a zero-overhead way to measure OCaml heap usage by reading
GC counters from the Runtime_events ring buffer. No GC pause required —
counters are updated by the runtime during normal GC cycles.

Usage: Initialize once at startup with [init_runtime_events ()], then
call [poll_runtime_events] and [heap_usage_mb ()] between batches.

Example:
  let init = Runtime_events_mem.init_runtime_events () in
  (* ... run solver ... *)
  Searchspace.poll_runtime_events ();
  let usage = Searchspace.heap_usage_mb () in
  if usage > 8000. then (* 8GB threshold *) greedy_mode ()

Requires OCaml 5.x with Runtime_events module and
OCAML_RUNTIME_EVENTS_START=1 environment variable. *)

(* Cursor for reading runtime events from the current process *)
let cursor : Runtime_events.cursor option ref = ref None

(* Counters - updated by callbacks during read_poll *)
let pool_live_words : int ref = ref 0
let large_words : int ref = ref 0

(* Initialize Runtime_events cursor. Call once at startup.
   Returns true if runtime events are available, false otherwise. *)
let init_runtime_events () : bool =
  (* Start runtime events if not already started by OCAML_RUNTIME_EVENTS_START *)
  (try Runtime_events.start () with _ -> ());
  
  match Runtime_events.path () with
  | Some path ->
      Printf.eprintf "[RuntimeEvents] ring buffer found at: %s\n" path;
      flush stderr;
      
      let c = Runtime_events.create_cursor None in
      cursor := Some c;
      true
  | None ->
      Printf.eprintf "[RuntimeEvents] NOT available (no ring buffer found)\n";
      Printf.eprintf "[RuntimeEvents] Run with OCAML_RUNTIME_EVENTS_START=1\n";
      flush stderr;
      false

(* Poll Runtime_events and update counter refs. Call between batches or before
   checking memory usage. This reads the latest counters from the ring buffer. *)
let poll_runtime_events () : unit =
  match !cursor with
  | Some c ->
      (* Don't reset counters here. read_poll only updates them when it reads
         counter events from the ring buffer. If no new events are available,
         we keep the last known values rather than dropping to 0. *)
      let counter_cb _domain_id _ts counter value =
        match counter with
        | Runtime_events.EV_C_MAJOR_HEAP_POOL_LIVE_WORDS -> pool_live_words := value
        | Runtime_events.EV_C_MAJOR_HEAP_LARGE_WORDS -> large_words := value
        | _ -> ()  (* ignore other counters *)
      in
      
      let cbs = Runtime_events.Callbacks.create ~runtime_counter:counter_cb () in
      ignore (Runtime_events.read_poll c cbs None)
  | None -> ()

(* Total live memory in OCaml words.
   Includes both small objects (pool) and large allocations.
   Calls poll_runtime_events internally to ensure fresh counters,
   since the ring buffer may have been updated by GCs between polls. *)
let heap_usage_words () : int =
  poll_runtime_events ();
  !pool_live_words + !large_words

(* Total live memory in megabytes. *)
let heap_usage_mb () : float =
  Float.of_int (heap_usage_words ()) *. Float.of_int Sys.word_size /. (1024.0 *. 1024.0)

(* Cleanup at program end *)
let shutdown_runtime_events () : unit =
  match !cursor with
  | Some c -> Runtime_events.free_cursor c
  | None -> ()
