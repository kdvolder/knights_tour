open Lib
let runtime task =
  let t_start = Unix.gettimeofday () in
  task ();
  let t_end = Unix.gettimeofday () in
  t_end -. t_start

let print_runtime task =
  let time = runtime task in
  Format.printf "time taken %fs\n" time

(* some sample tasks below *)

let solve_recursive size () = let open Horsejump in 
  GameState.make size 
  |> Horsejump.solve |> Option.get 
  |> (fun solution -> print_endline (Board.to_string solution))
let solve_searchspace size () = let open Horsejump in 
  make_search_space size 
  |> Searchspace.search |> Option.get 
  |> (fun (solution, _) -> print_endline (Board.to_string solution))
  
let () = 
  print_endline "******* solve_recursive ******";
  print_runtime (solve_recursive 8);
  print_endline "******* solve_searchspace ******";
  print_runtime (solve_searchspace 8)
