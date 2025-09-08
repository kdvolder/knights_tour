let format_number n =
  if n >= 1e10 || (n > 0. && n < 1e-3) then
    Printf.sprintf "%.5e" n
  else
    let s = Int64.to_string (Int64.of_float n) in
    let len = String.length s in
    let rec insert_commas i acc =
      if i <= 0 then acc
      else
        let sep = if (len - i) mod 3 = 0 && i <> len then "," else "" in
        insert_commas (i-1) (sep ^ String.sub s (i-1) 1 ^ acc)
    in
    insert_commas len ""
(* estimate_polyomino.ml
   Runs the stochastic estimator on a polyomino puzzle file.
   Usage: ./estimate_polyomino <puzzle-file>
*)


open Pentominos

let batch_size = 100

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <puzzle-file>\n" Sys.argv.(0);
    exit 1
  end;
  let puzzle_file = Sys.argv.(1) in
  let puzzle =
    In_channel.with_open_text puzzle_file Puzzle.load
  in
  let searchspace = Puzzle.solve puzzle in
  let estimator = Stochastic_estimator.create ~selector:Stochastic_estimator.variance_selector searchspace in
  while true do
    Stochastic_estimator.sample batch_size estimator;
    let est = Stochastic_estimator.estimates estimator in
    Printf.printf "Batch complete.\n";
    let percent_materialized =
      if est.nodes > 0. then (float_of_int est.materialized_nodes /. est.nodes) *. 100.
      else 0.
    in
    Printf.printf "  materialized nodes: %d (%.2e %%)\n" est.materialized_nodes percent_materialized;
    Printf.printf "  estimated nodes: %s\n" (format_number est.nodes);
    Printf.printf "  estimated fails: %s\n" (format_number est.fails);
    Printf.printf "  estimated solutions: %s\n" (format_number est.solutions);
    flush stdout
  done;
  let est = Stochastic_estimator.estimates estimator in
  Printf.printf "Interrupted. Final stats:\n";
  let percent_materialized =
    if est.nodes > 0. then (float_of_int est.materialized_nodes /. est.nodes) *. 100.
    else 0.
  in
  Printf.printf "  materialized nodes: %d (%.2e %%)\n" est.materialized_nodes percent_materialized;
  Printf.printf "  estimated nodes: %s\n" (format_number est.nodes);
  Printf.printf "  estimated fails: %s\n" (format_number est.fails);
  Printf.printf "  estimated solutions: %s\n" (format_number est.solutions);
  flush stdout
