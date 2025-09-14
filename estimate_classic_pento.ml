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
  
  let est = Stochastic_estimator.create ~selector:Stochastic_estimator.uniform_selector pentomino_space in
  
  Printf.printf "*** FOCUSING ON SEED 7 - THE BIG DROP ***\n";
  Printf.printf "Sample 1 -> 2: 187,649 nodes -> 5,390 nodes (97%% drop)\n\n";
  
  for i = 1 to 20 do
    Printf.printf "=== TAKING SAMPLE %d ===\n" i;
    ignore (Stochastic_estimator.sample 1 est);
    let current_estimates = Stochastic_estimator.estimates est in
    let percentage = (float_of_int current_estimates.materialized_nodes) /. (float_of_int true_values.nodes) *. 100.0 in
    let fail_plus_sol = current_estimates.fails +. current_estimates.solutions in
    Printf.printf "After %d samples: nodes=%.0f, fails=%.1f, solutions=%.1f, mat=%d (%.3f%%) [fail+sol=%.1f]\n"
      i current_estimates.nodes current_estimates.fails current_estimates.solutions 
      current_estimates.materialized_nodes percentage fail_plus_sol;
    Printf.printf "\n";
  done;
  
  Printf.printf "Debug complete.\n"
