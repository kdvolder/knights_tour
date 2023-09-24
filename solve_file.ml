(** Executable that Lloads puzzle problem from a 'polymino-puzzle.txt' file in the current directory. 
    Then starts solving it*)

open Pentominos

module Treequence = Searchspace__.Treequence

(* TODO: we smuch of this code is just copied from 'hexo_solve' we should try to modularize this better. *)

let load_file path =
  In_channel.with_open_text path Puzzle.load

let puzzle = load_file "polymino-puzzle.txt"

let stats_file = "stats.csv"

type stats = {
  steps: int;
  pop_ends: int;
  stack_size: int;
  solutions: int;
}
let stats = ref {steps=0;pop_ends=0;stack_size=0; solutions=0}

let print_solution i board = 
  print_endline ((Int.to_string (i + 1)) ^ ":");
  print_endline (Board.to_string board);
  stats := {
    !stats with solutions = !stats.solutions + 1;
  }


let last_event = ref 0.0

let rate_limit force doit =
  let event = Unix.time () in
  let elapsed = event -. !last_event in
  if elapsed > 2.0 || force then (
    last_event := event;
    doit ()
  )

let new_csv_progress_reporter file interval =
  let out = Out_channel.open_text file 
    |> Format.formatter_of_out_channel 
  in
  function (total_steps, {steps; pop_ends; stack_size; solutions}) -> 
    if total_steps mod interval = 0 then begin
      let branch_factor = Float.of_int stack_size /. Float.of_int pop_ends in
      let solve_ratio = Float.of_int total_steps /. Float.of_int solutions in
      Format.fprintf out "%d,%d,%d,%d,%f,%d,%f\n%!" total_steps steps pop_ends stack_size branch_factor solutions solve_ratio
    end


let new_graphical_progress_reporter csv_progress puzzle =
  let sz = Board.size puzzle.Puzzle.board in
  let draw_sz = Board.draw_size in
  Board.init_graphics sz;
  let best = ref Int.max_int in
  let steps = ref 0 in
  fun _ Puzzle.{board;pieces} -> (
    steps := !steps + 1;
    csv_progress (!steps, !stats);
    let pieces_left = List.length pieces in
    if pieces_left <= !best || !steps mod 100_000 = 0 then (
      rate_limit (pieces_left=0) (fun () ->
        best := pieces_left;
        Board.draw board;
        Graphics.moveto (12*draw_sz/2) (15*draw_sz/2);
        Graphics.set_color Graphics.white;
        Graphics.draw_string (Printf.sprintf "%d / %d / %d" !stats.steps !stats.stack_size !stats.pop_ends);
        Graphics.moveto (12*draw_sz/2) (14*draw_sz/2);
        if !stats.pop_ends>0 then (
          Graphics.draw_string (Printf.sprintf "%.4f" ((Float.of_int !stats.stack_size)/.(Float.of_int (!stats.pop_ends))))
        );
      )
    )
  )

let simple_progress_reporter csv_progress =  
  let best = ref Int.max_int in
  let steps = ref 0 in
  fun _ Puzzle.{board;pieces} -> begin
    steps := !steps + 1;
    csv_progress (!steps, !stats);
    let pieces_left = List.length pieces in
    if pieces_left <= !best || !steps mod 100_000 = 0 then begin
      rate_limit (pieces_left=0) (fun () ->
        best := pieces_left;
        let out = open_out "snapshot.txt" in
        Printf.fprintf out "%s\n" (Board.to_string board);
        Printf.fprintf out "%d: %d / %d / %d\n" !steps !stats.steps !stats.stack_size !stats.pop_ends; 
        if !stats.pop_ends>0 then (
          Printf.fprintf out "%.4f" ((Float.of_int !stats.stack_size)/.(Float.of_int (!stats.pop_ends)))
        );
        close_out out
      )  
    end
  end

let stack_mon msg steps stack = stats := {
  !stats with
  steps; 
  stack_size=Treequence.size stack;
  pop_ends = if msg="pop_end" then !stats.pop_ends+1 else !stats.pop_ends  
}

let useGraphics = ref false

let arg_specs = [
  ("--graphics", Arg.Set useGraphics, "Show progress in a Graphics window")
]

let usage_msg = "solve_file [--graphics]"

let memory_limit = Searchspace.limit_on_low_memory ~max_memory_ratio:0.95
  
let () =
  Arg.parse arg_specs (fun _ -> ()) usage_msg;
  let csv_progress = new_csv_progress_reporter stats_file 10_000 in
  Printf.printf "Use graphics? %b\n%!" !useGraphics;
  let progress_reporter = (if !useGraphics 
      then new_graphical_progress_reporter csv_progress puzzle
      else simple_progress_reporter csv_progress
  ) in
  Puzzle.solve ~report_progress:progress_reporter puzzle 
  |> Searchspace.to_seq ~search:(Searchspace.breadth_search ~limit:memory_limit ~stack_mon)
  |> Seq.iteri print_solution
