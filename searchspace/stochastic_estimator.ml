open Searchspace
open Collections.Util

type decision = {
    chosen: int;
    choices: int
}

type rng = int -> int

let ( let* ) = bind

let rec random_walk rng space = inspect space |> function
  | Fail -> ([], None)
	| Result x -> ([], Some x)
	| Fork choices -> 
			let num_choices = List.length choices in
			if num_choices==0 then
				([], None)
			else if num_choices==1 then
				let only_choice = List.hd choices in
				random_walk rng only_choice
			else (
				let chosen = rng num_choices in
				let chosen_el = List.nth choices chosen in
				let (recursed_path, result) = random_walk rng chosen_el in
				let path = {chosen;choices = num_choices}::recursed_path in
				(path, result)
			)

let decision_to_string {chosen;choices} =
	Int.to_string chosen ^ "/" ^ Int.to_string choices

let result_to_string to_string = function
	| Some x -> "Found: " ^ to_string x
	| None -> "Failed"

let walk_to_string to_string (path, result) =
	"[" 
	^ with_separator decision_to_string ", " path ^ 
	"] => " 
	^ result_to_string to_string result

let%expect_test "random_walk" = begin
	Random.full_init [|0|];
	let sums = (
		let* n1 = int_range 1 5 in 
		let* n2 = int_range 1 5 in
		let sum = n1+n2 in 
			(* if sum mod 2==0 then *)
				return (Printf.sprintf "%d + %d = %d" n1 n2 sum)
			(* else
				empty *)
	) in
	let do_test rng =
		let walk = random_walk rng sums in (
			Printf.printf "%s\n" (walk_to_string Fun.id walk)
		) 
	in (
		Printf.printf "Always first: ";
		do_test (fun _ -> 0);
		for _i=1 to 10 do
			Printf.printf("Random: ");
			do_test Random.int
		done;
		Printf.printf "Always last: ";
		do_test (fun bound -> bound-1)
	)
	; [%expect{|
		Always first: [0/2, 0/2] => Found: 1 + 1 = 2
		Random: [0/2, 0/2] => Found: 1 + 1 = 2
		Random: [1/2, 0/2, 0/2] => Found: 2 + 1 = 3
		Random: [0/2, 0/2] => Found: 1 + 1 = 2
		Random: [1/2, 0/2, 1/2, 1/2, 1/2, 0/2] => Found: 2 + 4 = 6
		Random: [0/2, 1/2, 1/2, 1/2, 1/2, 1/2] => Failed
		Random: [0/2, 0/2] => Found: 1 + 1 = 2
		Random: [1/2, 0/2, 0/2] => Found: 2 + 1 = 3
		Random: [0/2, 1/2, 0/2] => Found: 1 + 2 = 3
		Random: [0/2, 1/2, 1/2, 0/2] => Found: 1 + 3 = 4
		Random: [0/2, 1/2, 1/2, 1/2, 0/2] => Found: 1 + 4 = 5
		Always last: [1/2, 1/2, 1/2, 1/2, 1/2] => Failed |}]
end

let%expect_test "decision tree for sums divisible by 7" =
		let open Searchspace in
		let pp = pp_decision_tree (Format.pp_print_string) Format.std_formatter in
		let sums_div7 =
			let* n1 = int_range 1 4 in
			let* n2 = int_range 1 5 in
			let sum = n1 + n2 in
			if sum mod 7 = 0 then return (Printf.sprintf "%d + %d = %d" n1 n2 sum)
			else empty
		in begin
			sums_div7 |> pp
		end;
	[%expect{|
   choices
     choices
       FAIL
       choices
         FAIL
         choices
           FAIL
           choices
             FAIL
             choices
               FAIL
               FAIL
     choices
       choices
         FAIL
         choices
           FAIL
           choices
             FAIL
             choices
               FAIL
               choices
                 2 + 5 = 7
                 FAIL
       choices
         choices
           FAIL
           choices
             FAIL
             choices
               FAIL
               choices
                 3 + 4 = 7
                 choices
                   FAIL
                   FAIL
         choices
           choices
             FAIL
             choices
               FAIL
               choices
                 4 + 3 = 7
                 choices
                   FAIL
                   choices
                     FAIL
                     FAIL
           FAIL
   |}]

type stats = {
    nodes : int;
    forks : int;
    fails : int;
    solutions : int;
}

let rec calculate_true_values space = inspect space |> function
	| Result _ -> {nodes=1; forks=0; solutions=1; fails=0}
	| Fail -> {nodes=1; forks=0; solutions=0; fails=1}
	| Fork choices ->
			let children_stats = List.map calculate_true_values choices in
			let nodes = 1 + List.fold_left (fun acc s -> acc + s.nodes) 0 children_stats in
			let forks = 1 + List.fold_left (fun acc s -> acc + s.forks) 0 children_stats in
			let solutions = List.fold_left (fun acc s -> acc + s.solutions) 0 children_stats in
			let fails = List.fold_left (fun acc s -> acc + s.fails) 0 children_stats in
			{nodes; forks; solutions; fails}

let sums_div7 =
	let* n1 = int_range 1 4 in
	let* n2 = int_range 1 5 in
	let sum = n1 + n2 in
	if sum mod 7 = 0 then return (Printf.sprintf "%d + %d = %d" n1 n2 sum)
	else empty

type 'a node = {
	 node_view : 'a Searchspace.node_view;           (* Cached inspected view of the searchspace *)
	 mutable isCompleted : bool;										 (* Indicates if the node has been fully explored *)
	 mutable children : 'a node option array;        (* Children indexed by decision number; only some may be materialized *)
	 mutable samples : int;                          (* Number of samples passing through this node *)
	 mutable nodes_estimate : float;                 (* Current best estimate for subtree size *)
   mutable fail_estimate : float;                  (* Final estimate for failures in this subtree *)
	 mutable solution_estimate : float;              (* Final estimate for solutions in this subtree *)
	 mutable materialized_nodes : int;               (* Number of materialized nodes in this subtree *)
	 mutable pruned_nodes : int;                     (* Number of nodes freed when this node was pruned *)
}

let child_average (children : 'a node option array) (f : 'a node -> float) :
	float =
	let materialized = Array.to_list children |> List.filter_map (fun c -> c) in
	match materialized with
	| [] -> 0.0
	| xs -> List.fold_left (fun acc child -> acc +. f child) 0.0 materialized /. float_of_int (List.length xs)

let children_estimate (children : 'a node option array) (f : 'a node -> float) : float =
	float_of_int (Array.length children) *. child_average children f

let num_choices node_view = match node_view with
	| Fork choices -> List.length choices
	| _ -> 0

let create_node (space : 'a Searchspace.t) : 'a node =
		let node_view = inspect space in
		let (nodes_estimate, fail_estimate, solution_estimate, materialized_nodes, isCompleted, samples) = match node_view with
			| Result _ -> (1.0, 0.0, 1.0, 1, true, 1)   (* leaf nodes are created as fully sampled *)
			| Fail    -> (1.0, 1.0, 0.0, 1, true, 1)   (* leaf nodes are created as fully sampled *)
			| Fork _  -> (1.0, 0.0, 0.0, 1, false, 0)  (* initial values for forks, will be updated by sampling *)
		in {
			node_view;
			isCompleted;
			children = Array.make (num_choices node_view) None;
			samples;
			nodes_estimate;
			fail_estimate;
			solution_estimate;
			materialized_nodes;
			pruned_nodes = 0;
		}

type 'a child_selector = 'a node -> int

let uniform_selector node =
	Random.int (Array.length node.children)

let sample_rate = function
	| Some child -> float_of_int child.samples /. (child.fail_estimate +. child.solution_estimate)
	| None -> 0.0

let undersampled_selector (node : 'a node) : int =
	let n = Array.length node.children in
	if n = 0 then 0
	else
		let rates = Array.init n (fun i -> sample_rate node.children.(i)) in
		let min_rate = Array.fold_left min rates.(0) rates in
		let candidates = List.filter (fun i -> abs_float (rates.(i) -. min_rate) < 1e-8) (List.init n Fun.id) in
	List.nth candidates (Random.int (List.length candidates))


(* Select child with probability proportional to estimated unsampled leaves *)
let probabilistic_undersampled_selector (node : 'a node) : int =
  let n = Array.length node.children in
  if n = 0 then 0
  else
    let unmaterialized = List.filter (fun i -> node.children.(i) = None) (List.init n Fun.id) in
    if List.length unmaterialized > 0 then
      (* If any unmaterialized children, treat as infinite estimated children: pick one at random *)
      List.nth unmaterialized (Random.int (List.length unmaterialized))
    else (
      (* All children materialized: use unsampled-leaves-based selection *)
      let unsampled_leaves = Array.init n (fun i ->
        match node.children.(i) with
        | Some child ->
          let est_leaves = child.fail_estimate +. child.solution_estimate in
          Float.max 0. (est_leaves -. float_of_int child.samples)
        | None -> 0.0 (* unreachable, all materialized *)
      ) in
      let total = Array.fold_left ( +. ) 0.0 unsampled_leaves in
      if total = 0.0 then Random.int n
      else
        let r = Random.float total in
        let rec pick i acc =
          if i >= n then n - 1
          else if acc +. unsampled_leaves.(i) >= r then i
          else pick (i+1) (acc +. unsampled_leaves.(i))
        in pick 0 0.0
    )


let rec walk select_child on_solution (node : 'a node) : unit =
	match node.node_view with
	| Fail -> ()
	| Result x -> 
		(* Invoke callback for solution leaves before checking completion *)
		on_solution x
	| Fork choices ->
		if node.isCompleted then () 
		else (
			let num_choices = Array.length node.children in
			if num_choices > 0 then (
				let chosen = select_child node in
				let child_node = match node.children.(chosen) with
					| Some child -> child
					| None ->
						let c = create_node (List.nth choices chosen) in
						node.children.(chosen) <- Some c;
						c
				in
				walk select_child on_solution child_node;
				(* Calculate our sample count as sum of all child sample counts *)
				node.samples <- Array.fold_left (fun acc child_opt -> 
					match child_opt with 
					| Some child -> acc + child.samples 
					| None -> acc
				) 0 node.children;
				node.nodes_estimate <- 1. +. children_estimate node.children (fun child -> child.nodes_estimate);
				node.fail_estimate <- children_estimate node.children (fun child -> child.fail_estimate);
				node.solution_estimate <- children_estimate node.children (fun child -> child.solution_estimate);
				node.materialized_nodes <- 1 + Array.fold_left (fun acc child_opt -> match child_opt with Some child -> acc + child.materialized_nodes | None -> acc) 0 node.children;
				(* Update isCompleted: true if all children are materialized and themselves completed *)
				node.isCompleted <-
					Array.length node.children > 0 &&
					Array.for_all (function | Some c -> c.isCompleted | None -> false) node.children;
				(* Pruning hotspot: if all children completed, prune this node *)
				if node.isCompleted then (
					node.pruned_nodes <- node.materialized_nodes - 1;
					node.children <- [||]
				) else (
					node.pruned_nodes <- Array.fold_left (fun acc child_opt -> match child_opt with Some c -> acc + c.pruned_nodes | None -> acc) 0 node.children
				)
			)
	)

type estimates = {
	nodes : float;
	fails : float;
	solutions : float;
	materialized_nodes : int;
}

type leaf_type = Fail | Solution

type materialized_stats = {
	total_materialized : int;
	max_depth : int;
	leaf_depths_fail : (int * int) list;
	leaf_depths_solution : (int * int) list;
	fork_depths : (int * int) list;
	avg_leaf_depth_fail : float;
	avg_leaf_depth_solution : float;
}

let estimate ?(selector=undersampled_selector) n_trials (space : 'a Searchspace.t) : estimates =
	let root = create_node space in
	for _ = 1 to n_trials do
		ignore (walk selector (fun _ -> ()) root)
	done;
	{
		nodes = root.nodes_estimate;
		fails = root.fail_estimate;
		solutions = root.solution_estimate;
		materialized_nodes = root.materialized_nodes;
	}

let%expect_test "estimate number of nodes" =
	let true_values = calculate_true_values sums_div7 in
	Printf.printf "True values\n";
	Printf.printf "  number of nodes: %d\n" true_values.nodes;
	Printf.printf "  number of fails: %d\n" true_values.fails;
	Printf.printf "  number of solutions: %d\n" true_values.solutions;
	Printf.printf "\n";
	let estimates = estimate 1000 sums_div7 in
	Printf.printf "Estimated\n";
	Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
	Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
	Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
	Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
	[%expect{|
   True values
     number of nodes: 49
     number of fails: 22
     number of solutions: 3

   Estimated
     materialized nodes: 49
     number of nodes: 49
     number of fails: 22
     number of solutions: 3
   |}]

let rec balanced_range start stop =
	if start > stop then
		empty
	else if start = stop then
		return start
	else if start + 1 = stop then
		return start ++ return stop
	else
		let mid = (start + stop) / 2 in
		balanced_range start mid ++ balanced_range (mid + 1) stop

let%expect_test "undersampling larger balanced searchspace" =
	let int_range = balanced_range in
	let right_heavy_space = (
		let* n1 = int_range 1 100 in
		let* n2 = int_range 1 100 in
		let sum = return (n1 + n2) in 
		sum |?> (fun x -> x mod 7 = 0)
	) in
		let true_values = calculate_true_values right_heavy_space in
		Printf.printf "True values\n";
		Printf.printf "  number of nodes: %d\n" true_values.nodes;
		Printf.printf "  number of fails: %d\n" true_values.fails;
		Printf.printf "  number of solutions: %d\n" true_values.solutions;
		Printf.printf "\n";
		for samplers = 1 to 5 do
			let samples = 1000 * samplers in
			Printf.printf "Sample run %d:\n" samples;
			let estimates = estimate samples right_heavy_space in
			Printf.printf "Estimated values balanced trees:\n";
			Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
			Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
			Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
			Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
			Printf.printf "\n";
		done;
	[%expect{|
   True values
     number of nodes: 19999
     number of fails: 8572
     number of solutions: 1428

   Sample run 1000:
   Estimated values balanced trees:
     materialized nodes: 5143
     number of nodes: 19007
     number of fails: 8228
     number of solutions: 1276

   Sample run 2000:
   Estimated values balanced trees:
     materialized nodes: 8301
     number of nodes: 19187
     number of fails: 8278
     number of solutions: 1316

   Sample run 3000:
   Estimated values balanced trees:
     materialized nodes: 10568
     number of nodes: 18661
     number of fails: 8013
     number of solutions: 1318

   Sample run 4000:
   Estimated values balanced trees:
     materialized nodes: 12268
     number of nodes: 18439
     number of fails: 7984
     number of solutions: 1236

   Sample run 5000:
   Estimated values balanced trees:
     materialized nodes: 13598
     number of nodes: 17291
     number of fails: 7448
     number of solutions: 1198
   |}]


let%expect_test "undersampling larger unbalanced searchspace" =
	let right_heavy_space = (
		let* n1 = int_range 1 100 in
		let* n2 = int_range 1 100 in
		let sum = return (n1 + n2) in 
		sum |?> (fun x -> x mod 7 = 0)
	) in
		let true_values = calculate_true_values right_heavy_space in
		Printf.printf "True values\n";
		Printf.printf "  number of nodes: %d\n" true_values.nodes;
		Printf.printf "  number of fails: %d\n" true_values.fails;
		Printf.printf "  number of solutions: %d\n" true_values.solutions;
		Printf.printf "\n";
		for samplers = 1 to 5 do
			let samples = 1000 * samplers in
			Printf.printf "Sample run %d:\n" samples;
			let estimates = estimate samples right_heavy_space in
			Printf.printf "Estimated values (unbalanced trees):\n";
			Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
			Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
			Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
			Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
			Printf.printf "\n";
		done;
	[%expect{|
   True values
     number of nodes: 20201
     number of fails: 8673
     number of solutions: 1428

   Sample run 1000:
   Estimated values (unbalanced trees):
     materialized nodes: 2099
     number of nodes: 2199
     number of fails: 946
     number of solutions: 154

   Sample run 2000:
   Estimated values (unbalanced trees):
     materialized nodes: 4100
     number of nodes: 4203
     number of fails: 1798
     number of solutions: 304

   Sample run 3000:
   Estimated values (unbalanced trees):
     materialized nodes: 6100
     number of nodes: 6203
     number of fails: 2651
     number of solutions: 451

   Sample run 4000:
   Estimated values (unbalanced trees):
     materialized nodes: 8099
     number of nodes: 8199
     number of fails: 3514
     number of solutions: 586

   Sample run 5000:
   Estimated values (unbalanced trees):
     materialized nodes: 10099
     number of nodes: 10199
     number of fails: 4378
     number of solutions: 722
   |}]

(** Incremental estimator API implementation *)
type 'a t = {
	root : 'a node;
	selector : 'a child_selector;
	on_solution : 'a -> unit;
}

let create ?(selector=undersampled_selector) ?(on_solution=(fun _ -> ())) (space : 'a Searchspace.t) : 'a t =
	{ root = create_node space; selector; on_solution }

let sample n (est : 'a t) : bool =
	let rec loop n =
		if n <= 0 || est.root.isCompleted then ()
		else (
			ignore (walk est.selector est.on_solution est.root);
			loop (n-1)
		)
	in loop n; est.root.isCompleted

let estimates (est : 'a t) : estimates =
	{
		nodes = est.root.nodes_estimate;
		fails = est.root.fail_estimate;
		solutions = est.root.solution_estimate;
		materialized_nodes = est.root.materialized_nodes;
	}

let analyze_materialized (est : 'a t) : materialized_stats =
	(* Walk the materialized tree and collect depth statistics. Read-only - no new materialization. *)
	let fail_depths = ref [] in
	let sol_depths = ref [] in
	let fork_depths = ref [] in
	let rec walk depth node =
		match node.node_view with
		| Result _ -> sol_depths := (depth, 1) :: !sol_depths
		| Fail -> fail_depths := (depth, 1) :: !fail_depths
		| Fork _ ->
			fork_depths := (depth, 1) :: !fork_depths;
			Array.iter (function
				| Some child -> walk (depth + 1) child
				| None -> () (* unmaterialized child - skip *)
			) node.children
	in
	walk 0 est.root;
	let total_materialized =
		List.fold_left (fun acc (_, c) -> acc + c) 0 !fail_depths
		+ List.fold_left (fun acc (_, c) -> acc + c) 0 !sol_depths
		+ List.fold_left (fun acc (_, c) -> acc + c) 0 !fork_depths
	in
	let max_depth =
		(List.map fst !fail_depths @ List.map fst !sol_depths
		 @ List.map fst !fork_depths) |> function
		| [] -> 0 | d :: ds -> List.fold_left max d ds
	in
	let avg_leaf_depth_fail =
		match !fail_depths with
		| [] -> 0.0
		| _ ->
			let total = List.fold_left (fun acc (d, c) -> acc +. float_of_int d *. float_of_int c) 0.0 !fail_depths in
			let count = List.fold_left (fun acc (_, c) -> acc + c) 0 !fail_depths in
			total /. float_of_int count
	in
	let avg_leaf_depth_solution =
		match !sol_depths with
		| [] -> 0.0
		| _ ->
			let total = List.fold_left (fun acc (d, c) -> acc +. float_of_int d *. float_of_int c) 0.0 !sol_depths in
			let count = List.fold_left (fun acc (_, c) -> acc + c) 0 !sol_depths in
			total /. float_of_int count
	in
	{ total_materialized; max_depth; leaf_depths_fail = !fail_depths; leaf_depths_solution = !sol_depths; fork_depths = !fork_depths; avg_leaf_depth_fail; avg_leaf_depth_solution }

(** Progress monitoring *)
type progress = {
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  progress_percent : float;
  estimated_remaining_seconds : float;
}

let make_progress (start_time : float) (est : 'a t) : progress =
  let now = Unix.gettimeofday () in
  let elapsed = now -. start_time in
  let ests = estimates est in
  let progress_percent =
    if ests.nodes > 0. then
      (float_of_int ests.materialized_nodes) /. ests.nodes *. 100.0
    else 0.0
  in
  let estimated_remaining =
    if progress_percent > 0. && progress_percent < 100. then
      elapsed *. (100.0 /. progress_percent) -. elapsed
    else if progress_percent >= 100. then
      0.0
    else
      Float.infinity
  in
  {
    elapsed_seconds = elapsed;
    total_nodes_estimate = ests.nodes;
    fails_estimate = ests.fails;
    solutions_estimate = ests.solutions;
    materialized_nodes = ests.materialized_nodes;
    progress_percent;
    estimated_remaining_seconds = estimated_remaining;
  }

type time_components = {
  years : float;
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let rec format_time (seconds : float) : string =
  if seconds < 0. then "-" ^ format_time (-.seconds)
  else
    let years_float = seconds /. 31536000. in
    if years_float > 1e9 then
      string_of_float years_float ^ " years"
    else
      let total = int_of_float seconds in
      let comps = {
        years = float_of_int (total / 31536000);
        days = (total mod 31536000) / 86400;
        hours = (total mod 86400) / 3600;
        minutes = (total mod 3600) / 60;
        seconds = total mod 60;
      } in
      let parts = ref [] in
      if comps.years > 0. then parts := (if comps.years = 1. then "1 year" else string_of_int (int_of_float comps.years) ^ " years") :: !parts;
      if comps.days > 0 then parts := (if comps.days = 1 then "1 day" else string_of_int comps.days ^ " days") :: !parts;
      if comps.hours > 0 then parts := (string_of_int comps.hours ^ " h") :: !parts;
      if comps.minutes > 0 then parts := (string_of_int comps.minutes ^ " min") :: !parts;
      if comps.seconds > 0 then parts := (string_of_int comps.seconds ^ " s") :: !parts;
      if !parts = [] then parts := ["0 s"];
      let result = ref "" in
      let sep = ref "" in
      List.iter (fun part ->
        if !sep = "" then result := part
        else result := !result ^ !sep ^ part;
        sep := if String.ends_with ~suffix:"day" part || String.ends_with ~suffix:"year" part then ", " else " "
      ) (List.rev !parts);
      !result

let default_progress_printer (p : progress) : unit =
  let eta_str =
    if p.progress_percent >= 100. then "done"
    else if Float.is_infinite p.estimated_remaining_seconds then "inf"
    else format_time p.estimated_remaining_seconds
  in
  Printf.printf "[%5.1f%%] materialized: %d, elapsed: %s, ETA: %s\n" p.progress_percent p.materialized_nodes (format_time p.elapsed_seconds) eta_str;
  flush stdout

let run_with_progress ?(batch_size = 100) ?(on_progress = default_progress_printer) (est : 'a t) : unit =
  let start_time = Unix.gettimeofday () in
  let rec loop () =
    if not est.root.isCompleted then (
      ignore (sample batch_size est);
      let p = make_progress start_time est in
      on_progress p;
      loop ()
    )
  in
  loop ();
  (* Final report when complete *)
  let p = make_progress start_time est in
  on_progress p

let%expect_test "incremental estimator API on unbalanced searchspace" =
  let right_heavy_space = (
    let* n1 = int_range 1 100 in
    let* n2 = int_range 1 100 in
    let sum = return (n1 + n2) in
    sum |?> (fun x -> x mod 7 = 0)
  ) in
  let true_values = calculate_true_values right_heavy_space in
  Printf.printf "True values\n";
  Printf.printf "  number of nodes: %d\n" true_values.nodes;
  Printf.printf "  number of fails: %d\n" true_values.fails;
  Printf.printf "  number of solutions: %d\n" true_values.solutions;
  Printf.printf "\n";
  let est = create right_heavy_space in
	let completed = ref false in
	let samplers = ref 1 in
	while not !completed && !samplers <= 5 do
    let samples = 1000 * !samplers in
    completed := sample 1000 est;
    Printf.printf "Sample run %d:\n" samples;
    let estimates = estimates est in
    Printf.printf "Estimated values (incremental):\n";
    Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
    Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
    Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
    Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
    Printf.printf "\n";
		samplers := !samplers + 1
  done;
  [%expect{|
    True values
      number of nodes: 20201
      number of fails: 8673
      number of solutions: 1428

    Sample run 1000:
    Estimated values (incremental):
      materialized nodes: 2099
      number of nodes: 2199
      number of fails: 946
      number of solutions: 154

    Sample run 2000:
    Estimated values (incremental):
      materialized nodes: 4101
      number of nodes: 4211
      number of fails: 1798
      number of solutions: 308

    Sample run 3000:
    Estimated values (incremental):
      materialized nodes: 6099
      number of nodes: 6199
      number of fails: 2665
      number of solutions: 435

    Sample run 4000:
    Estimated values (incremental):
      materialized nodes: 8099
      number of nodes: 8199
      number of fails: 3515
      number of solutions: 585

    Sample run 5000:
    Estimated values (incremental):
      materialized nodes: 10099
      number of nodes: 10199
      number of fails: 4372
      number of solutions: 728
    |}]

let%expect_test "sample counting verification" = begin
  Random.full_init [|42|];
  
  (* Create a simple test tree: Root with 3 children *)
  let simple_tree = Searchspace.(
	alt [
		return "Child 0";
		alt [
			return "Grandchild 0";
			return "Grandchild 1"
		];
		empty
	]
  ) in

  let estimator = create simple_tree in
  
  Printf.printf "Before sampling:\n";
  Printf.printf "Root samples: %d\n" estimator.root.samples;
  
  let completed = sample 1 estimator in
  
  Printf.printf "After 1 sample:\n";
  Printf.printf "Root samples: %d (should be 1)\n" estimator.root.samples;
  Printf.printf "Completed: %b\n" completed;
  
  (* Walk and print the tree structure with sample counts *)
  let rec print_tree indent node =
    match node.node_view with
    | Result s -> Printf.printf "%sResult '%s' [samples=%d]\n" indent s node.samples
    | Fail -> Printf.printf "%sFail [samples=%d]\n" indent node.samples
    | Fork _ -> 
        Printf.printf "%sFork [samples=%d]\n" indent node.samples;
        Array.iteri (fun i child_opt ->
          match child_opt with
          | Some child -> 
              Printf.printf "%s  Child %d:\n" indent i;
              print_tree (indent ^ "    ") child
          | None -> 
              Printf.printf "%s  Child %d: not materialized\n" indent i
        ) node.children
  in
  
  Printf.printf "\nTree structure:\n";
  print_tree "" estimator.root
end; 
[%expect{|
  Before sampling:
  Root samples: 0
  After 1 sample:
  Root samples: 1 (should be 1)
  Completed: false

  Tree structure:
  Fork [samples=1]
    Child 0: not materialized
    Child 1:
      Fork [samples=1]
        Child 0:
          Result 'Grandchild 0' [samples=1]
        Child 1: not materialized
    Child 2: not materialized
  |}]

let%expect_test "oversampling behavior with uniform selector" = begin
  Random.full_init [|42|];
  
  (* Create a simple test tree: Root with 3 children *)
  let simple_tree = Searchspace.(
	alt [
		return "Child 0";
		alt [
			return "Grandchild 0";
			return "Grandchild 1"
		];
		empty
	]
  ) in

  let estimator = create ~selector:uniform_selector simple_tree in
  
  Printf.printf "=== Initial state ===\n";
  Printf.printf "Root samples: %d\n" estimator.root.samples;
  
  (* Walk and print the tree structure with sample counts *)
  let rec print_tree indent node =
    match node.node_view with
    | Result s -> Printf.printf "%sResult '%s' [samples=%d, completed=%b]\n" indent s node.samples node.isCompleted
    | Fail -> Printf.printf "%sFail [samples=%d, completed=%b]\n" indent node.samples node.isCompleted
    | Fork _ -> 
        Printf.printf "%sFork [samples=%d, completed=%b]\n" indent node.samples node.isCompleted;
        Array.iteri (fun i child_opt ->
          match child_opt with
          | Some child -> 
              Printf.printf "%s  Child %d:\n" indent i;
              print_tree (indent ^ "    ") child
          | None -> 
              Printf.printf "%s  Child %d: not materialized\n" indent i
        ) node.children
  in
  
  (* Sample multiple times to show oversampling behavior *)
  for i = 1 to 5 do
    let completed = sample 1 estimator in
    Printf.printf "\n=== After %d sample(s) ===\n" i;
    Printf.printf "Root completed: %b\n" completed;
    print_tree "" estimator.root;
    Printf.printf "\n"
  done
end; 
[%expect{|
  === Initial state ===
  Root samples: 0

  === After 1 sample(s) ===
  Root completed: false
  Fork [samples=1, completed=false]
    Child 0: not materialized
    Child 1:
      Fork [samples=1, completed=false]
        Child 0:
          Result 'Grandchild 0' [samples=1, completed=true]
        Child 1: not materialized
    Child 2: not materialized


  === After 2 sample(s) ===
  Root completed: false
  Fork [samples=1, completed=false]
    Child 0: not materialized
    Child 1:
      Fork [samples=1, completed=false]
        Child 0:
          Result 'Grandchild 0' [samples=1, completed=true]
        Child 1: not materialized
    Child 2: not materialized


  === After 3 sample(s) ===
  Root completed: false
  Fork [samples=2, completed=false]
    Child 0:
      Result 'Child 0' [samples=1, completed=true]
    Child 1:
      Fork [samples=1, completed=false]
        Child 0:
          Result 'Grandchild 0' [samples=1, completed=true]
        Child 1: not materialized
    Child 2: not materialized


  === After 4 sample(s) ===
  Root completed: false
  Fork [samples=2, completed=false]
    Child 0:
      Result 'Child 0' [samples=1, completed=true]
    Child 1:
      Fork [samples=1, completed=false]
        Child 0:
          Result 'Grandchild 0' [samples=1, completed=true]
        Child 1: not materialized
    Child 2: not materialized


  === After 5 sample(s) ===
  Root completed: false
  Fork [samples=2, completed=false]
    Child 0:
      Result 'Child 0' [samples=1, completed=true]
    Child 1:
      Fork [samples=1, completed=false]
        Child 0:
          Result 'Grandchild 0' [samples=1, completed=true]
        Child 1: not materialized
    Child 2: not materialized
  |}]

(** Phase 1: API Design Tests - verify the callback API compiles and works *)

let%expect_test "create without callback still works" = begin
  let simple_tree = Searchspace.(
    alt [
      return "Child 0";
      alt [ return "Grandchild 0"; return "Grandchild 1" ];
      empty
    ]
  ) in
  let est = create simple_tree in
  Printf.printf "Created estimator without callback\n";
  Printf.printf "Root samples: %d\n" est.root.samples;
  [%expect{|
    Created estimator without callback
    Root samples: 0
  |}]
end

let%expect_test "create with callback compiles" = begin
  let simple_tree = Searchspace.(
    alt [
      return "Child 0";
      alt [ return "Grandchild 0"; return "Grandchild 1" ];
      empty
    ]
  ) in
  let callback_count = ref 0 in
  let on_solution x =
    incr callback_count;
    Printf.printf "Callback received: %s\n" x
  in
  let est = create ~on_solution simple_tree in
  Printf.printf "Created estimator with callback\n";
  Printf.printf "Root samples: %d\n" est.root.samples;
  [%expect{|
    Created estimator with callback
    Root samples: 0
  |}]
end

(** Phase 2: Callback Invocation Tests - verify callback fires correctly *)

let%expect_test "callback called on solution found during sample" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions found: %d\n" (List.length !solutions);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions found: 1
    Solutions: solution_a
  |}]
end

let%expect_test "callback receives correct solution value" = begin
  let simple_tree = Searchspace.(
    alt [
      return "first";
      return "second";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: first, second
  |}]
end

let%expect_test "callback not called for failures" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: solution
  |}]
end

let%expect_test "callback not called when no solutions exist" = begin
  let simple_tree = Searchspace.(
    alt [
      empty;
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions:
  |}]
end

(** Phase 3: Integration Tests - callback works across selectors and multiple sample() calls *)

let%expect_test "callback works with undersampled_selector" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~selector:undersampled_selector ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: sol_a, sol_b
  |}]
end

let%expect_test "callback works with probabilistic_undersampled_selector" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~selector:probabilistic_undersampled_selector ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: sol_a, sol_b
  |}]
end

let%expect_test "callback works across multiple sample() calls" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 1 est);
  Printf.printf "After sample 1: %s\n" (String.concat ", " !solutions);
  ignore (sample 1 est);
  Printf.printf "After sample 2: %s\n" (String.concat ", " !solutions);
  ignore (sample 10 est);
  Printf.printf "After sample 3: %s\n" (String.concat ", " !solutions);
  [%expect{|
    After sample 1:
    After sample 2: sol_a
    After sample 3: sol_a, sol_b
  |}]
end

let%expect_test "callback receives solutions in sampling order" = begin
  let simple_tree = Searchspace.(
    alt [
      return "first";
      return "second";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  Random.full_init [|12345|];
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: first, second
  |}]
end

(** Task 2 Phase 1: Progress Data Structure Tests *)

let%expect_test "make_progress initial state" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 1
    progress%: 100.0
  |}]
end

let%expect_test "make_progress after sampling" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  ignore (sample 10 est);
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 4
    progress%: 100.0
  |}]
end

(** Task 2 Phase 2: Time Formatting Tests *)

let%expect_test "format_time: seconds" = begin
  Printf.printf "%s\n" (format_time 0.0);
  Printf.printf "%s\n" (format_time 5.0);
  Printf.printf "%s\n" (format_time 42.0);
  [%expect{|
    0 s
    5 s
    42 s
  |}]
end

let%expect_test "format_time: minutes and seconds" = begin
  Printf.printf "%s\n" (format_time 59.0);
  Printf.printf "%s\n" (format_time 60.0);
  Printf.printf "%s\n" (format_time 142.0);
  [%expect{|
    59 s
    1 min
    2 min 22 s
  |}]
end

let%expect_test "format_time: hours" = begin
  Printf.printf "%s\n" (format_time 3600.0);
  Printf.printf "%s\n" (format_time 7530.0);
  [%expect{|
    1 h
    2 h 5 min 30 s
  |}]
end

let%expect_test "format_time: days" = begin
  Printf.printf "%s\n" (format_time 86400.0);
  Printf.printf "%s\n" (format_time 150125.0);
  [%expect{|
    1 day
    1 day, 17 h 42 min 5 s
  |}]
end

let%expect_test "format_time: larger units" = begin
  Printf.printf "%s\n" (format_time 31536000.0); (* ~1 year *)
  [%expect{|
    1 year
  |}]
end

let%expect_test "format_time: astronomical years" = begin
  Printf.printf "%s\n" (format_time 1e20); (* ~3 billion years *)
  Printf.printf "%s\n" (format_time 1e30); (* ~31 trillion years *)
  Printf.printf "%s\n" (format_time 1e50); (* ~3.2e41 years *)
  [%expect{|
    3.17097919838e+12 years
    3.17097919838e+22 years
    3.17097919838e+42 years
  |}]
end

(** Task 2 Phase 3: Reporter Integration Tests *)

let%expect_test "run_with_progress invokes callback after each batch" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:3 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|
    Reports: 4, 4
  |}]
end

let%expect_test "run_with_progress uses default stdout printer" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:3 est;
  [%expect{|
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
  |}]
end

let%expect_test "run_with_progress stops when complete" = begin
  let simple_tree = Searchspace.(
    alt [ return "sol"; empty ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:5 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|
    Final materialized: 3
  |}]
end

(** Task 6: Tree Pruning Tests *)

(* Helper to inspect node children variant and pruned_nodes count *)
let rec print_tree indent (node : 'a node) =
  let pruned_marker = if Array.length node.children = 0 && node.isCompleted then " **PRUNED**" else "" in
  let output = indent ^ "Fork [samples=" ^ string_of_int node.samples 
                          ^ " nodes=" ^ string_of_float node.nodes_estimate
                          ^ " completed=" ^ string_of_bool node.isCompleted 
                          ^ " materialized=" ^ string_of_int node.materialized_nodes 
                          ^ " pruned=" ^ string_of_int node.pruned_nodes ^ "]" ^ pruned_marker in
  Printf.printf "%s\n" output;
  Array.iteri (fun i child_opt ->
    match child_opt with
    | Some child -> Printf.printf "%s  Child %d:\n" indent i; print_tree (indent ^ "    ") child
    | None -> Printf.printf "%s  Child %d: not materialized\n" indent i
  ) node.children

(* Play-around test: simple search space to inspect pruning propagation *)
let%expect_test "debug: pick two numbers 1..3, sum > 4" = begin
  let num = of_list [1;2;3] in
  let simple_space =
     num |=> (fun x -> 
       num |=> (fun y -> 
        return (x + y)
       )
     ) 
     |?> (fun sum -> sum>4) in 
  let est = create simple_space in
  let samples = 8 in
  
  Printf.printf "=== Before sampling ===\n";
  print_tree "" est.root;
  
  Random.full_init [|42|];
  ignore (sample samples est);
  Printf.printf "\n=== After %d samples ===\n" samples;
  print_tree "" est.root;
  [%expect{|
    === Before sampling ===
    Fork [samples=0 nodes=1. completed=false materialized=1 pruned=0]
      Child 0: not materialized
      Child 1: not materialized
      Child 2: not materialized

    === After 8 samples ===
    Fork [samples=8 nodes=13. completed=false materialized=12 pruned=6]
      Child 0:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2:
        Fork [samples=2 nodes=4. completed=false materialized=3 pruned=0]
          Child 0: not materialized
          Child 1:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 2:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
    |}]
end

let%expect_test "oversampling is fine" = begin 
  let num = of_list [1;2;3] in
  let simple_space =
     num |=> (fun x -> 
       num |=> (fun y -> 
        return (x + y)
       )
     ) 
     |?> (fun sum -> sum>4) in 
  let solutions = ref [] in
  let on_solution x = solutions:= x :: !solutions in
  let est = create ~on_solution simple_space in
  sample 100 est |> ignore;
  Printf.printf "=== solutions ===\n";
  List.iter (Printf.printf "%d; ") !solutions;
  Printf.printf "\n===== tree ====\n";
  print_tree "" est.root;
  [%expect{|
    === solutions ===
    6; 5; 5;
    ===== tree ====
    Fork [samples=9 nodes=13. completed=true materialized=13 pruned=12] **PRUNED**
    |}]
end

let%expect_test "create with callback compiles" = begin
  let simple_tree = Searchspace.(
    alt [
      return "Child 0";
      alt [ return "Grandchild 0"; return "Grandchild 1" ];
      empty
    ]
  ) in
  let callback_count = ref 0 in
  let on_solution x =
    incr callback_count;
    Printf.printf "Callback received: %s\n" x
  in
  let est = create ~on_solution simple_tree in
  Printf.printf "Created estimator with callback\n";
  Printf.printf "Root samples: %d\n" est.root.samples;
  [%expect{|
    Created estimator with callback
    Root samples: 0
  |}]
end

(** Phase 2: Callback Invocation Tests - verify callback fires correctly *)

let%expect_test "callback called on solution found during sample" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions found: %d\n" (List.length !solutions);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions found: 1
    Solutions: solution_a
  |}]
end

let%expect_test "callback receives correct solution value" = begin
  let simple_tree = Searchspace.(
    alt [
      return "first";
      return "second";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: first, second
  |}]
end

let%expect_test "callback not called for failures" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: solution
  |}]
end

let%expect_test "callback not called when no solutions exist" = begin
  let simple_tree = Searchspace.(
    alt [
      empty;
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions:
  |}]
end

(** Phase 3: Integration Tests - callback works across selectors and multiple sample() calls *)

let%expect_test "callback works with undersampled_selector" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~selector:undersampled_selector ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: sol_a, sol_b
  |}]
end

let%expect_test "callback works with probabilistic_undersampled_selector" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~selector:probabilistic_undersampled_selector ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: sol_a, sol_b
  |}]
end

let%expect_test "callback works across multiple sample() calls" = begin
  let simple_tree = Searchspace.(
    alt [
      return "sol_a";
      return "sol_b";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  let est = create ~on_solution simple_tree in
  ignore (sample 1 est);
  Printf.printf "After sample 1: %s\n" (String.concat ", " !solutions);
  ignore (sample 1 est);
  Printf.printf "After sample 2: %s\n" (String.concat ", " !solutions);
  ignore (sample 10 est);
  Printf.printf "After sample 3: %s\n" (String.concat ", " !solutions);
  [%expect{|
    After sample 1:
    After sample 2: sol_a
    After sample 3: sol_a, sol_b
  |}]
end

let%expect_test "callback receives solutions in sampling order" = begin
  let simple_tree = Searchspace.(
    alt [
      return "first";
      return "second";
      empty
    ]
  ) in
  let solutions = ref [] in
  let on_solution x = solutions := !solutions @ [x] in
  Random.full_init [|12345|];
  let est = create ~on_solution simple_tree in
  ignore (sample 10 est);
  Printf.printf "Solutions: %s\n" (String.concat ", " !solutions);
  [%expect{|
    Solutions: first, second
  |}]
end

(** Task 2 Phase 1: Progress Data Structure Tests *)

let%expect_test "make_progress initial state" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 1
    progress%: 100.0
  |}]
end

let%expect_test "make_progress after sampling" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  ignore (sample 10 est);
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 4
    progress%: 100.0
  |}]
end

(** Task 2 Phase 2: Time Formatting Tests *)

let%expect_test "format_time: seconds" = begin
  Printf.printf "%s\n" (format_time 0.0);
  Printf.printf "%s\n" (format_time 5.0);
  Printf.printf "%s\n" (format_time 42.0);
  [%expect{|
    0 s
    5 s
    42 s
  |}]
end

let%expect_test "format_time: minutes and seconds" = begin
  Printf.printf "%s\n" (format_time 59.0);
  Printf.printf "%s\n" (format_time 60.0);
  Printf.printf "%s\n" (format_time 142.0);
  [%expect{|
    59 s
    1 min
    2 min 22 s
  |}]
end

let%expect_test "format_time: hours" = begin
  Printf.printf "%s\n" (format_time 3600.0);
  Printf.printf "%s\n" (format_time 7530.0);
  [%expect{|
    1 h
    2 h 5 min 30 s
  |}]
end

let%expect_test "format_time: days" = begin
  Printf.printf "%s\n" (format_time 86400.0);
  Printf.printf "%s\n" (format_time 150125.0);
  [%expect{|
    1 day
    1 day, 17 h 42 min 5 s
  |}]
end

let%expect_test "format_time: larger units" = begin
  Printf.printf "%s\n" (format_time 31536000.0); (* ~1 year *)
  [%expect{|
    1 year
  |}]
end

let%expect_test "format_time: astronomical years" = begin
  Printf.printf "%s\n" (format_time 1e20); (* ~3 billion years *)
  Printf.printf "%s\n" (format_time 1e30); (* ~31 trillion years *)
  Printf.printf "%s\n" (format_time 1e50); (* ~3.2e41 years *)
  [%expect{|
    3.17097919838e+12 years
    3.17097919838e+22 years
    3.17097919838e+42 years
  |}]
end

(** Task 2 Phase 3: Reporter Integration Tests *)

let%expect_test "run_with_progress invokes callback after each batch" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:3 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|
    Reports: 4, 4
  |}]
end

let%expect_test "run_with_progress uses default stdout printer" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:3 est;
  [%expect{|
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
  |}]
end

let%expect_test "run_with_progress stops when complete" = begin
  let simple_tree = Searchspace.(
    alt [ return "sol"; empty ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:5 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|
    Final materialized: 3
  |}]
end
