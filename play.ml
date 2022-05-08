open Knights_tour.Game
open GameState

let rec do_until prod =
  match prod () with
  | None -> do_until prod
  | Some x -> x

let choose_move moves = do_until (fun () ->
  print_endline ("Choose a move [" ^ moves_to_string moves ^ "]: ");
  let input = read_line () in
  print_endline ("Chosen '" ^ input ^ "'");
  let chosen = List.find_opt (fun move -> String.equal (move_to_string move) input) moves in
  ( match chosen with
  | None -> print_endline "That move is not valid"
  | Some _ -> print_endline "Good move"
  );
  chosen
)

let rec play game_state = 
  print_endline (Board.to_string (board game_state));
  let moves = GameState.valid_moves game_state in
  if moves==[] then
    print_endline "=== no more moves ===" 
  else 
    GameState.do_move game_state (choose_move moves);
    play game_state

let () = play (GameState.make 8) 
