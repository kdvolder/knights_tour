open Knights_tour
open Pentominos

let puzzle = Puzzle.classic_no_symmetric_solutions

let print_solution i board = 
  print_endline ((Int.to_string (i + 1)) ^ ":");
  print_endline (Board.to_string board)

(* let report_progress kind puzzle =
  print_endline ("Progress: " ^ kind);
  print_endline ("Remaining pieces: " ^ (Puzzle.(puzzle.pieces) |> List.length |> Int.to_string));
  print_endline (Board.to_string Puzzle.(puzzle.board));
  print_endline "Press enter to continue...";
  read_line () |> fun _ -> () *)

(* let new_progress_reporter () =
  let best = ref Int.max_int in
  let steps = ref 0 in
  fun kind Puzzle.{board;pieces} ->
    steps := !steps + 1;
    let pieces_left = List.length pieces in
    if pieces_left < !best then begin
      best := pieces_left;
      print_endline ("Progress: " ^ kind);
      print_endline ("Steps: " ^ Int.to_string !steps);
      print_endline (Board.to_string board)
    end *)

let new_graphical_progress_reporter puzzle =
  let sz = Board.size puzzle.Puzzle.board in
  Graphics.open_graph (Printf.sprintf " %dx%d" (sz.x * 32) (sz.y * 32));
  let best = ref Int.max_int in
  fun _ Puzzle.{board;pieces} -> (
    let pieces_left = List.length pieces in
    if pieces_left <= !best then (
      best := pieces_left;
      Board.draw board;
      Unix.sleepf 0.025
    )
  )

let () =
  Puzzle.solve ~report_progress:(new_graphical_progress_reporter puzzle) puzzle 
  |> Searchspace.to_seq
  |> Seq.iteri print_solution
