(** Small experiment: does OCaml return memory to the OS?

Usage: ./test_gc_mem.exe [compact:true|false]
  compact:true = calls Gc.compact() after shrinking
  compact:false = does NOT call Gc.compact() (default)

Control: write target MB to /tmp/test_gc_mem_target
  e.g. "1024" = try to use ~1GB, "0" = free everything

Monitor: watch RSS with 'ps -p <pid> -o rss' or check /proc/meminfo

Example:
  ./test_gc_mem.exe compact:true &
  echo "2048" > /tmp/test_gc_mem_target   # grow to ~2GB
  echo "512" > /tmp/test_gc_mem_target    # shrink to ~512MB
  echo "0" > /tmp/test_gc_mem_target      # free everything
*)

let chunk_size_mb = 10
let tolerance_mb = 5.0  (* MB tolerance band *)
let control_file = "/tmp/test_gc_mem_target"

(* Allocate a chunk of memory ~chunk_size_mb MB *)
let sizeof_int = 8 (* bytes per int on 64-bit *)

let allocate_chunk () =
  let words = (chunk_size_mb * 1024 * 1024) / sizeof_int in
  Array.make words '\x00'

let allocated = ref []

let read_target () : float option =
  try
    let ch = In_channel.open_text control_file in
    match In_channel.input_line ch with
    | Some line -> close_in ch; Some (Float.of_string line)
    | None -> close_in ch; None
  with _ -> None

let mem_used_mb () : float =
  let ch = In_channel.open_text "/proc/self/status" in
  try
    let rec loop () =
      match In_channel.input_line ch with
      | Some line ->
          let colon_pos = String.index line ':' in
          let key = String.sub line 0 colon_pos in
          if key = "VmRSS" then begin
            let rest = String.trim (String.sub line (colon_pos + 1) (String.length line - colon_pos - 1)) in
            let space_pos = String.index rest ' ' in
            let kb_str = String.sub rest 0 space_pos in
            close_in ch;
            Float.of_string kb_str /. 1024.0
          end else loop ()
      | None -> close_in ch; 0.0
    in
    loop ()
  with _ ->
    close_in ch;
    0.0

let shrink () =
  match !allocated with
  | [] -> ()
  | _ :: rest -> allocated := rest

let rec loop () =
  match read_target () with
  | Some target ->
      let current_mb = mem_used_mb () in
      if (current_mb +. tolerance_mb) < target then begin
        allocated := allocate_chunk () :: !allocated;
      end else if target < current_mb -. tolerance_mb then begin
        shrink ();
        Gc.compact ()  (* only if compact mode *)
      end else begin
        Unix.sleep 2;
      end;
      loop ()
  | None ->
      Unix.sleep 1;
      loop ()

let compact_mode = ref false

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "compact:true" then
    compact_mode := true;

  Printf.printf "PID: %d\n" (Unix.getpid ());
  Printf.printf "Compact mode: %b\n" !compact_mode;
  Printf.printf "Write target MB to %s\n" control_file;
  Printf.printf "Monitor with: ps -p %d -o rss,etime\n" (Unix.getpid ());
  flush stdout;

  loop ()
