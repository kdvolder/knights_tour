open Knights_tour.Game

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

let () =
    let board_size = 8 in
    Graphics.open_graph (Printf.sprintf " %dx%d" (board_size * 32) (board_size * 32));
    solve ~report_backtracking (GameState.make board_size) |> report_solution;
    print_endline "Press any key to EXIT";
    let _ = read_line () in
    print_endline "Goodbye!"
