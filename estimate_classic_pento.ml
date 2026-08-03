let () =
  Printf.printf "=== Classic Pentomino SEED 7 Debug ===\n\n";
  
  (* Focus on SEED 7 which shows the dramatic drop *)
  Random.full_init [|7|];
  
  (* Use the actual classic pentomino puzzle *)
  let pentomino_puzzle = Pentominos.Puzzle.classic_no_symmetric_solutions in
  let pentomino_space = Pentominos.Puzzle.solve pentomino_puzzle in
  
  Printf.printf "Classic pentomino puzzle board:\n%s\n" 
    (Pentominos.Board.to_string pentomino_puzzle.board);
  
  (* Calculate true values for comparison *)
  Printf.printf "Calculating true values (this may take a while)...\n";
  let hash_pentomino_space _space =
    (* Create a simple hash based on the search space structure *)
    let digest = Digest.string "classic_pentomino_space" in
    Digest.to_hex digest
  in
  let calculate_true_values_cached = Knights_tour.Cache.memoize
    ~cache_dir:"/tmp/pentomino_cache"
    ~function_name:"calculate_true_values"
    ~hash:hash_pentomino_space
    Stochastic_estimator.calculate_true_values
  in
  let true_values = calculate_true_values_cached pentomino_space in
  Printf.printf "True values: nodes=%d, fails=%d, solutions=%d\n\n" 
    true_values.nodes true_values.fails true_values.solutions;
  
  let solution_count = ref 0 in
  let on_solution board =
    incr solution_count;
    Printf.printf "\n*** Solution #%d ***\n%s\n" !solution_count (Pentominos.Board.to_string board)
  in
  let est = Stochastic_estimator.create 
    ~selector:Stochastic_estimator.undersampled_selector
    ~on_solution
    pentomino_space in
    
  let batch_size = 5000 in
  Stochastic_progress.run_with_progress ~batch_size ~on_progress:(fun p ->
    let current_estimates = Stochastic_estimator.estimates est in
    let stats = Stochastic_estimator.analyze_materialized est in
    
    Printf.printf "[%.1f%%] ETA: %s\n" (p.progress_ratio *. 100.0) (Table_logger.format_time 20 p.estimated_remaining_seconds);
    Printf.printf "=== BATCH ===\n";
    Printf.printf "Estimates: nodes=%.0f, fails=%.1f, solutions=%.1f\n" 
      current_estimates.nodes current_estimates.fails current_estimates.solutions;
    Printf.printf "Materialized: %d nodes\n" stats.total_materialized;
    
    Printf.printf "Depth | Fails (mat) | Solutions (mat)\n";
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
    Printf.printf "\n%!";
    true
  ) est;
  