(** Generic log-cycling pipeline.

    Treats lines as opaque strings — the cycling mechanic doesn't care what's in them.
    Each level is a file that holds at most 100 entries (50 oldest + 50 newest).
    When a file overflows, its oldest 50 entries are compressed (keep every 10th = ~5)
    and cascaded to the next level. Levels are created lazily as data cascades into them.

    Normal case (99% of the time): append a line to L0's file. Done.
    Overflow case (rare): read the file, split oldest/newest 50, compress oldest,
    rewrite L0's file with header + newest 50, cascade ~5 lines to next level.

    The recursion is elegant — overflow cascades to the next level, which may itself
    overflow and cascade further. Each level is identical in structure and behavior.

    Autoflushes after every [add_line] for crash safety (via open_out_gen with buffering).
*)

type t = {
  stamp : string;
  header : string;
  dir : string option;               (** Optional base directory for log files. *)
  first : logfile option ref;        (** First level (L0), chain via next fields. *)
}

and logfile = {
  level : int;
  filename : string;
  pipeline_header : string;               (** Pipeline header for rewriting. *)
  mutable channel : out_channel option;   (** Open for append. *)
  mutable line_count : int;               (** Lines in file — kept in memory so we don't have to read the file every time. *)
  next : logfile option ref;              (** Next level, created lazily on overflow. *)
}

let create ~stamp ~header ?dir () = {
  stamp;
  header;
  dir;
  first = ref None;
}

(** Create a logfile for the given level, writing the header line. *)
let make_logfile t level =
  let basename = Printf.sprintf "logs-%s-L%d.csv" t.stamp level in
  let filename = match t.dir with
    | None -> basename
    | Some d -> Filename.concat d basename
  in
  let ch = open_out filename in
  output_string ch t.header;
  output_char ch '\n';
  flush ch;
  {
    level;
    filename;
    pipeline_header = t.header;
    channel = Some ch;
    line_count = 0;
    next = ref None;
  }

let read_all_lines fname = 
  let ch = open_in fname in
  let lines = ref [] in
  try
    while true do
      lines := input_line ch :: !lines
    done
  with End_of_file -> close_in ch;
  List.rev !lines


(** Read all lines from a file (excluding header). Returns empty list if file is
    just a header or doesn't exist. *)
let read_data_lines (filename : string) : string List.t =
  read_all_lines filename |> List.drop 1

(** Keep every 10th line from the input list. *)
let compress lines =
  lines |> List.filteri (fun i _ -> i mod 10 = 0)

  
let write_line_unconditional current line : unit =
  (* add line to open channel, increment line count*)
  (match current.channel with
   | Some ch -> output_string ch line; output_char ch '\n'
   | None -> failwith "write_line_unconditional: channel is None");
  current.line_count <- current.line_count + 1

let rec compress_and_overflow (t : t) current : unit =
  (* 1: close the channel *)
  (match current.channel with Some ch -> close_out ch | None -> ());
  
  (* 2: read all the lines in the file *)
  let all_lines = read_data_lines current.filename in
  
  (* 3: split into oldest 50 and newest 50 *)
  let old_compressed = List.take 50 all_lines |> compress in
  let newest_50 = List.drop 50 all_lines in
    
  (* 6: open new file for overwrite (empty + header) *)
  let ch = (open_out current.filename) in
  current.channel <- Some ch;
  output_string ch t.header;
  output_char ch '\n';
  current.line_count <- 0;
  
  (* 7: write newest 50 lines *)
  List.iter (fun line -> write_line_unconditional current line) newest_50;
    
  (* 5: cascade compressed lines to next level *)
  List.iter (fun line -> add_line_to_file t (current.level + 1) current.next line) old_compressed 

(** Add a line to a logfile, checks for overflow and does overflow handling if needed.
    On overflow: read file, split oldest/newest 50, compress oldest ~5 lines,
    rewrite current with newest 50, cascade compressed to next level. *)
and add_line_to_file t level (target : logfile option ref) (line : string) : unit =
  (* Lazy creation: if no logfile yet, create one *)
  if Option.is_none !target then (
    target := Some (make_logfile t level)
  );
  
  let current = Option.get !target in
  write_line_unconditional current line;
  if current.line_count > 100 then (
    compress_and_overflow t current
  );
  current.channel |> Option.iter flush
  
let rec close_log_files current_ref =
  !current_ref |> Option.iter (fun current -> 
    current.channel |> Option.iter close_out;
    current.next |> close_log_files
  )  

(** Close the pipeline and all open channels. *)
let close t = close_log_files t.first

let add_line t = add_line_to_file t 0 t.first

(* ============================================================================ *)
(* Test helpers                                                                 *)
(* ============================================================================ *)

let print_file_summary fname =
  let lines = read_all_lines fname in
  Printf.printf "%s:\n---\n" fname;
  let show = List.take 5 lines in
  List.iter (fun l -> Printf.printf "%s\n" l) show;
  if List.length lines > 10 then (
    Printf.printf "...\n";
    let show = List.drop (List.length lines - 5) lines in
    List.iter (fun l -> Printf.printf "%s\n" l) show
  ) else (
    let show = List.drop 5 lines in
    List.iter (fun l -> Printf.printf "%s\n" l) show
  );
  Printf.printf "---\n"

(* ============================================================================ *)
(* Tests                                                                        *)
(* ============================================================================ *)

let%expect_test "multi_log many lines" =
  let stamp = "2025-06-15-14-30" in
  (* Clean up any leftover files from previous runs *)
  let existing = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter Sys.remove existing;
  let header = "level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta" in
  let pipeline = create ~stamp ~header () in
  
  (* Shove 1250 lines into L0 — triggers overflow at line 101, cascades ~5 to L1 *)
  for i = 1 to 1200 do
    add_line pipeline (Printf.sprintf "line-%d" i)
  done;
  
  close pipeline;
  
  (* Print summaries of all log files *)
  let files = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter print_file_summary (List.sort String.compare files);
  
  (* Cleanup *)
  List.iter Sys.remove files;
  [%expect {|
    logs-2025-06-15-14-30-L0.csv:
    ---
    level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta
    line-1101
    line-1102
    line-1103
    line-1104
    ...
    line-1196
    line-1197
    line-1198
    line-1199
    line-1200
    ---
    logs-2025-06-15-14-30-L1.csv:
    ---
    level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta
    line-501
    line-511
    line-521
    line-531
    ...
    line-1051
    line-1061
    line-1071
    line-1081
    line-1091
    ---
    logs-2025-06-15-14-30-L2.csv:
    ---
    level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta
    line-1
    line-101
    line-201
    line-301
    line-401
    ---
    |}]

(* ============================================================================ *)
(* Autoflush tests — validate files are written WITHOUT calling close           *)
(* ============================================================================ *)

let%expect_test "multi_log autoflush single line" =
  let stamp = "2025-06-15-14-31" in
  let existing = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter Sys.remove existing;
  let header = "HEADER" in
  let pipeline = create ~stamp ~header () in
  
  (* Add a single line — no close yet *)
  add_line pipeline "hello,world";
  
  (* Check file exists and has content WITHOUT calling close *)
  let files = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter print_file_summary (List.sort String.compare files);
  
  (* Cleanup *)
  List.iter Sys.remove files;
  [%expect {|
    logs-2025-06-15-14-31-L0.csv:
    ---
    HEADER
    hello,world
    ---
    |}]

let%expect_test "multi_log autoflush overflow" =
  let stamp = "2025-06-15-14-32" in
  let existing = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter Sys.remove existing;
  let header = "col1,col2" in
  let pipeline = create ~stamp ~header () in
  
  (* Add 101 lines — triggers overflow at line 101, cascades ~5 to L1 *)
  for i = 1 to 101 do
    add_line pipeline (Printf.sprintf "line-%d" i)
  done;
  
  (* Check BOTH files exist and are correct WITHOUT calling close *)
  let files = Sys.readdir "." |> Array.to_list |> List.filter (fun f ->
    String.contains f 'L' && String.ends_with ~suffix:".csv" f
  ) in
  List.iter print_file_summary (List.sort String.compare files);
  
  (* Cleanup *)
  List.iter Sys.remove files;
  [%expect {|
    logs-2025-06-15-14-32-L0.csv:
    ---
    col1,col2
    line-51
    line-52
    line-53
    line-54
    ...
    line-97
    line-98
    line-99
    line-100
    line-101
    ---
    logs-2025-06-15-14-32-L1.csv:
    ---
    col1,col2
    line-1
    line-11
    line-21
    line-31
    line-41
    ---
    |}]
