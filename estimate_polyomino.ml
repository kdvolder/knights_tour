open Pentominos

let format_number n =
  if n >= 1e10 || (n > 0. && n < 1.) then
    Printf.sprintf "%.5e" n
  else
    Int64.to_string (Int64.of_float n)

let batch_size = 5000

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
  let estimator = Stochastic_estimator.create ~selector:Stochastic_estimator.probabilistic_undersampled_selector searchspace in
  let space_complete = ref false in
  let samples = ref 0 in
  Printf.printf "Batch | Samples | Nodes Est  | Fails Est  | Sols Est | Materialized | %%Complete\n";
  Printf.printf "------|---------|------------|------------|----------|-------------|----------\n";
  let batch = ref 1 in
  while not !space_complete do
    space_complete := Stochastic_estimator.sample batch_size estimator;
    samples := !samples + batch_size;
    let est = Stochastic_estimator.estimates estimator in
    let percent_complete =
      if est.nodes > 0. then (float_of_int est.materialized_nodes /. est.nodes) *. 100.
      else 0.
    in
    Printf.printf "%5d | %7d | %10s | %10s | %8s | %11d | %10.2e%%\n%!"
      !batch !samples (format_number est.nodes) (format_number est.fails) (format_number est.solutions) est.materialized_nodes percent_complete;
    batch := !batch + 1;
  done;
