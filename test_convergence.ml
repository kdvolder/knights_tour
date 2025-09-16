let () =
  Printf.printf "=== Pentomino Convergence Test ===\n\n";
  
  (* Use SEED 7 which showed the problematic behavior *)
  Random.full_init [|7|];
  
  let pentomino_puzzle = Pentominos.Puzzle.classic_no_symmetric_solutions in
  let pentomino_space = Pentominos.Puzzle.solve pentomino_puzzle in
  
  Printf.printf "Classic pentomino puzzle board:\n%s\n" 
    (Pentominos.Board.to_string pentomino_puzzle.board);
  
  (* Calculate true values for comparison *)
  Printf.printf "Calculating true values (this may take a while)...\n";
  let hash_pentomino_space _space =
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
  
  let batch_size = 10_000 in
  let est = Stochastic_estimator.create ~selector:Stochastic_estimator.probabilistic_undersampled_selector pentomino_space in
  
  Printf.printf "Testing convergence in batches of %d samples:\n" batch_size;
  Printf.printf "Batch | Samples | Nodes Est  | Fails Est  | Sols Est | Node Acc | Fail Acc | Sol Acc | Materialized\n";
  Printf.printf "------|---------|------------|------------|----------|----------|----------|---------|-------------\n";
  
  for batch = 1 to 24 do
    ignore (Stochastic_estimator.sample batch_size est);
    
    let current_estimates = Stochastic_estimator.estimates est in
    let node_accuracy = current_estimates.nodes /. (float_of_int true_values.nodes) *. 100.0 in
    let fail_accuracy = current_estimates.fails /. (float_of_int true_values.fails) *. 100.0 in
    let sol_accuracy = current_estimates.solutions /. (float_of_int true_values.solutions) *. 100.0 in
    let total_samples = batch * batch_size in
    
    Printf.printf "%5d | %7d | %10.0f | %10.0f | %8.1f | %7.1f%% | %7.1f%% | %6.1f%% | %11d\n%!"
      batch total_samples current_estimates.nodes current_estimates.fails current_estimates.solutions 
      node_accuracy fail_accuracy sol_accuracy current_estimates.materialized_nodes;
  done;
  
  Printf.printf "\nFinal comparison:\n";
  let final_estimates = Stochastic_estimator.estimates est in
  Printf.printf "True:      nodes=%d, fails=%d, solutions=%d\n" 
    true_values.nodes true_values.fails true_values.solutions;
  Printf.printf "Estimated: nodes=%.0f, fails=%.0f, solutions=%.0f\n"
    final_estimates.nodes final_estimates.fails final_estimates.solutions;
  Printf.printf "Accuracy:  %.1f%% (nodes)\n" 
    (final_estimates.nodes /. (float_of_int true_values.nodes) *. 100.0)
