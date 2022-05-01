open Lib
open Horsejump
open GameState

let report_solution = function
  | None ->  print_endline "==== NO SOLUTIONS ===="
  | Some board ->
      print_endline "==== SOLVED ====";
      print_endline (Board.to_string board);
      Board.draw board

let report_backtracking =
  let max_steps = ref 0 in
  let fails = ref 0 in
    fun state -> 
      fails := !fails + 1;
      let depth = GameState.steps state in
      let board = GameState.board state in 
      if depth > !max_steps then (
        max_steps := depth;
        print_endline "====================";
        Printf.printf "Backtrack [depth = %d] [fails = %d]\n%!" !max_steps !fails;
        print_endline (Board.to_string board);
        Board.draw board
      ) else if !fails mod 100_000==0 then (
        Board.draw board
      )

let rec solve state =
  if isWinning state then
    Some (GameState.board state)
  else
    match GameState.valid_moves state with
    | [] ->
        report_backtracking state;
        None
    | moves ->
        List.to_seq moves
        |> Seq.find_map (fun move -> try_move state move)
and try_move state move = 
  GameState.do_move state move;
  match solve state with
  | None -> GameState.undo_move state move; None
  | Some solution -> Some solution

let () =
    Graphics.open_graph (Printf.sprintf " %dx%d" (Board.size * 32) (Board.size * 32));
    solve (GameState.make ()) |> report_solution;
    print_endline "Press any key to EXIT";
    let _ = read_line () in
    print_endline "Goodbye!"
