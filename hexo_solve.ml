open Pentominos

module Treequence = Searchspace.Treequence

(* let puzzle = Puzzle.{
  board = Board.of_string "
    .........#.........  
    1224567890987654321
    2##################
    3##################
    4##################
    5##################
    6##################
    7##################
    8##################
    9##################
    0##################
    1##################
  ";
    pieces = Polyomino.of_order 6 |> Polyomino.randomize
} *)

let puzzle = Puzzle.{
  board = Board.of_string "
    122456789012345    
    2##############
    3##############
    4##############
    5##############
    6##############
    7####.....12345
    8####.....#####
    9####.....#####
    10#############
    11#############
    12#############
    13#############
    14#############
    15#############
  ";
    pieces = Polyomino.of_order 6 |> Polyomino.randomize
}

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


type stats = {
  steps: int;
  pop_ends: int;
  stack_size: int;
}    
let stats = ref {steps=0;pop_ends=0;stack_size=0}

let last_event = ref 0.0

let rate_limit force doit =
  let event = Unix.time () in
  let elapsed = event -. !last_event in
  if elapsed > 2.0 || force then (
    last_event := event;
    doit ()
  )

let new_graphical_progress_reporter puzzle =
  let sz = Board.size puzzle.Puzzle.board in
  let draw_sz = Board.draw_size in
  Board.init_graphics sz;
  let best = ref Int.max_int in
  let steps = ref 0 in
  fun _ Puzzle.{board;pieces} -> (
    steps := !steps + 1;
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

let stack_mon msg steps stack = stats := {
  steps; 
  stack_size=Treequence.size stack;
  pop_ends = if msg="pop_end" then !stats.pop_ends+1 else !stats.pop_ends  
}

let () =
  let bf () = 1.0 in
  Printf.printf "Breadth search with factor = %f\n%!" (bf ());
  Puzzle.solve ~report_progress:(new_graphical_progress_reporter puzzle) puzzle 
  |> Searchspace.to_seq ~search:(Searchspace.breadth_search ~limit:bf ~stack_mon)
  |> Seq.iteri print_solution
