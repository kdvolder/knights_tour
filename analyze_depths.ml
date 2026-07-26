open Printf

let rec collect_leaf_depths space depth acc = 
  match Searchspace.inspect space with
  | Searchspace.Result _ -> (depth, "solution") :: acc
  | Searchspace.Fail -> (depth, "fail") :: acc  
  | Searchspace.Fork choices ->
    List.fold_left (fun acc choice -> 
      collect_leaf_depths choice (depth + 1) acc
    ) acc choices

let analyze_depth_distribution () =
  let space = Pentominos.Puzzle.solve Pentominos.Puzzle.classic_no_symmetric_solutions in
  let leaf_data = collect_leaf_depths space 0 [] in
  
  (* Separate solutions from failures *)
  let solutions = List.filter (fun (_, t) -> t = "solution") leaf_data in
  let failures = List.filter (fun (_, t) -> t = "fail") leaf_data in
  
  printf "=== Leaf Node Depth Analysis ===\n";
  printf "Total leaves: %d (solutions: %d, failures: %d)\n" 
    (List.length leaf_data) (List.length solutions) (List.length failures);
  printf "\n";
  
  (* Find depth ranges *)
  let depths = List.map fst leaf_data in
  let min_depth = List.fold_left min (List.hd depths) depths in
  let max_depth = List.fold_left max (List.hd depths) depths in
  
  printf "Depth range: %d to %d\n\n" min_depth max_depth;
  
  (* Create histogram buckets *)
  let histogram = Hashtbl.create 50 in
  List.iter (fun (depth, leaf_type) ->
    let key = (depth, leaf_type) in
    let count = try Hashtbl.find histogram key with Not_found -> 0 in
    Hashtbl.replace histogram key (count + 1)
  ) leaf_data;
  
  printf "Depth | Failures | Solutions | Total\n";
  printf "------|----------|-----------|-------\n";
  
  for depth = min_depth to max_depth do
    let fail_count = try Hashtbl.find histogram (depth, "fail") with Not_found -> 0 in
    let sol_count = try Hashtbl.find histogram (depth, "solution") with Not_found -> 0 in
    let total = fail_count + sol_count in
    if total > 0 then
      printf "%5d | %8d | %9d | %5d\n" depth fail_count sol_count total
  done;
  
  printf "\n=== Solution Statistics ===\n";
  if List.length solutions > 0 then (
    let solution_depths = List.map fst solutions in
    let avg_sol_depth = (List.fold_left (+) 0 solution_depths |> float_of_int) /. float_of_int (List.length solutions) in
    let min_sol_depth = List.fold_left min (List.hd solution_depths) solution_depths in
    let max_sol_depth = List.fold_left max (List.hd solution_depths) solution_depths in
    printf "Solution depths: min=%d, max=%d, avg=%.1f\n" min_sol_depth max_sol_depth avg_sol_depth
  );
  
  printf "\n=== Failure Statistics ===\n";
  if List.length failures > 0 then (
    let failure_depths = List.map fst failures in
    let avg_fail_depth = (List.fold_left (+) 0 failure_depths |> float_of_int) /. float_of_int (List.length failures) in
    let min_fail_depth = List.fold_left min (List.hd failure_depths) failure_depths in
    let max_fail_depth = List.fold_left max (List.hd failure_depths) failure_depths in
    printf "Failure depths: min=%d, max=%d, avg=%.1f\n" min_fail_depth max_fail_depth avg_fail_depth
  )

let compare_with_materialized () = begin
  let space = Pentominos.Puzzle.solve Pentominos.Puzzle.classic_no_symmetric_solutions in
  let est = Stochastic_estimator.create ~selector:Stochastic_estimator.undersampled_selector space in
  let true_values = Stochastic_estimator.calculate_true_values space in
  Printf.printf "\n=== Materialized Tree Analysis (via analyze_materialized) ===\n";
  Printf.printf "True total nodes: %d\n" true_values.nodes;
  
  let batch_size = 1000 in
  let rec fully_materialize est count =
    ignore (Stochastic_estimator.sample batch_size est);
    let e = Stochastic_estimator.estimates est in
    if count mod (batch_size * 10) = 0 then (
      Printf.printf "  Batch %d: materialized=%d (%.1f%%)\n" count e.materialized_nodes 
        (float_of_int e.materialized_nodes /. float_of_int true_values.nodes *. 100.0)
    );
    if e.materialized_nodes >= true_values.nodes then ()
    else fully_materialize est (count + batch_size)
  in
  fully_materialize est 0;
  
  let stats = Stochastic_estimator.analyze_materialized est in
  Printf.printf "\nMaterialized nodes: %d\n" stats.total_materialized;
  Printf.printf "Avg fail depth: %.1f (count=%d)\n" stats.avg_leaf_depth_fail
    (List.fold_left (fun acc (_, c) -> acc + c) 0 stats.leaf_depths_fail);
  Printf.printf "Avg solution depth: %.1f (count=%d)\n" stats.avg_leaf_depth_solution
    (List.fold_left (fun acc (_, c) -> acc + c) 0 stats.leaf_depths_solution);
  Printf.printf "\nDepth | Fails (mat) | Solutions (mat)\n";
  Printf.printf "------|-------------|----------------\n";
  let fail_hist = Hashtbl.create 20 in
  List.iter (fun (d, c) -> Hashtbl.add fail_hist d ((Hashtbl.find_opt fail_hist d |> Option.value ~default:0) + c)) stats.leaf_depths_fail;
  let sol_hist = Hashtbl.create 20 in
  List.iter (fun (d, c) -> Hashtbl.add sol_hist d ((Hashtbl.find_opt sol_hist d |> Option.value ~default:0) + c)) stats.leaf_depths_solution;
  let all_depths = List.map fst stats.leaf_depths_fail @ List.map fst stats.leaf_depths_solution in
  let min_d = match all_depths with [] -> 0 | d :: ds -> List.fold_left min d ds in
  let max_d = match all_depths with [] -> 0 | d :: ds -> List.fold_left max d ds in
  for depth = min_d to max_d do
    let fail_count = Hashtbl.find_opt fail_hist depth |> Option.value ~default:0 in
    let sol_count = Hashtbl.find_opt sol_hist depth |> Option.value ~default:0 in
    Printf.printf "%5d | %9d | %13d\n" depth fail_count sol_count
  done;
  Printf.printf "\nCoverage: %.3f%% of true tree (%d / %d nodes)\n" 
    (float_of_int stats.total_materialized /. float_of_int true_values.nodes *. 100.0)
    stats.total_materialized true_values.nodes
end

let () = 
  analyze_depth_distribution ();
  compare_with_materialized ()
