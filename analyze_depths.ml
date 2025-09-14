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

let () = analyze_depth_distribution ()
