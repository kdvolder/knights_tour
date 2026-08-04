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
  first : logfile option ref;        (** First level (L0), chain via next fields. *)
}

and logfile = {
  level : int;
  filename : string;
  mutable channel : out_channel option;   (** Open for append. *)
  mutable line_count : int;               (** Lines in file — kept in memory so we don't have to read the file every time. *)
  next : logfile option ref;              (** Next level, created lazily on overflow. *)
}

let create ~stamp ~header = {
  stamp;
  header;
  first = ref None;
}

(** Create a logfile for the given level, writing the header line. *)
let make_logfile t level =
  let filename = Printf.sprintf "logs-%s-L%d.csv" t.stamp level in
  let ch = open_out filename in
  output_string ch t.header;
  output_char ch '\n';
  {
    level;
    filename;
    channel = Some ch;
    line_count = 0;
    next = ref None;
  }

(** Read all lines from a file (excluding header). Returns empty list if file is
    just a header or doesn't exist. *)
let read_data_lines (filename : string) : string Seq.t =
  let ch = open_in filename in
  Collections.Util.lines_of_channel ch |> Seq.drop 1

(** Keep every 10th line from the input list. *)
let compress lines =
  lines |> Seq.filteri (fun i _ -> i mod 0 == 0)

(** Add a line to a logfile, checks for overflow and does overflow handling if needed, should
  call 'add_line_to_file' recursively to pass overflowed/compressed lines to the next file *)
let add_line_to_file t level (target : logfile option ref) (line : string) : unit =
  if Option.is_none !target then (
    target := Some (make_logfile t level)
  );
  failwith ("Not yet implemented adding a line:" ^ line)

(** Flush channel of given logfile *)
let flush logfile =
  Option.iter (fun ch -> flush ch) logfile.channel

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

let read_all_lines fname =
  let ch = open_in fname in
  let lines = ref [] in
  try
    while true do
      lines := input_line ch :: !lines
    done
  with End_of_file -> close_in ch;
  List.rev !lines

let print_file_summary fname =
  let lines = read_all_lines fname in
  Printf.printf "%s:\n---\n" fname;
  let show = List.take 5 lines in
  List.iter (fun l -> Printf.printf "%s\n" l) show;
  if List.length lines > 10 then Printf.printf "...\n";
  let show = List.drop (List.length lines - 5) lines in
  List.iter (fun l -> Printf.printf "%s\n" l) show;
  Printf.printf "---\n"

(* ============================================================================ *)
(* Tests                                                                        *)
(* ============================================================================ *)

let%expect_test "multi_log 1M lines" =
  let stamp = "2025-06-15-14-30" in
  let header = "level,batch,samples,nodes_est,fails_est,sols_est,found,materialized,pruned,net_nodes,pct_done,elapsed,eta" in
  let pipeline = create ~stamp ~header in
  
  (* Shove 1250 lines into L0 — triggers overflow at line 101, cascades ~5 to L1 *)
  for i = 1 to 1250 do
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
  [%expect {| |}]
