open Searchspace
open Collections.Util

type 'a node = {
	 node_view : 'a Searchspace.node_view Lazy.t;    (* Deferred inspected view of the searchspace *)
	 mutable isCompleted : bool;										 (* Indicates if the node has been fully explored *)
	 mutable children : 'a node option array;        (* Children indexed by decision number; only some may be materialized *)
	 mutable samples : int;                          (* Number of samples passing through this node *)
	 mutable nodes_estimate : float;                 (* Current best estimate for subtree size *)
   mutable fail_estimate : float;                  (* Final estimate for failures in this subtree *)
	 mutable solution_estimate : float;              (* Final estimate for solutions in this subtree *)
	 mutable materialized_nodes : int;               (* Number of materialized nodes in this subtree *)
	 mutable pruned_nodes : int;                     (* Number of nodes freed when this node was pruned *)
}

type 'a child_selector = 'a t -> 'a node -> int
and  'a t = {
	root : 'a node;
	selector : 'a child_selector;
	on_solution : 'a -> unit;
}

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

type node_entry = {
  path : decision_path;
  num_choices : int;
  samples : int;
  nodes_estimate : float;
  fail_estimate : float;
  solution_estimate : float;
  materialized_nodes_count : int;
  pruned_nodes : int;
  is_completed : bool;
}

and decision_path = decision list

let create_node (space : 'a Searchspace.t) : 'a node =
		let node_view = inspect space in
		let (nodes_estimate, fail_estimate, solution_estimate, materialized_nodes, isCompleted, samples) = match node_view with
			| Result _ -> (1.0, 0.0, 1.0, 1, true, 1)   (* leaf nodes are created as fully sampled *)
			| Fail    -> (1.0, 1.0, 0.0, 1, true, 1)   (* leaf nodes are created as fully sampled *)
			| Fork _  -> (1.0, 0.0, 0.0, 1, false, 0)  (* initial values for forks, will be updated by sampling *)
		in {
			node_view = Lazy.from_val node_view;
			isCompleted;
			children = Array.make (num_choices node_view) None;
			samples;
			nodes_estimate;
			fail_estimate;
			solution_estimate;
			materialized_nodes;
			pruned_nodes = 0;
		}

(* Create a node with an unforced view. Stats are set from serialized data.
   Takes a lazy node_view so the expensive 'inspect' is deferred until actually needed. *)
let create_node_lazy (view : 'a Searchspace.node_view Lazy.t) (entry : node_entry) : 'a node = {
	node_view = view;
	isCompleted = entry.is_completed;
	children = Array.make entry.num_choices None;
	samples = entry.samples;
	nodes_estimate = entry.nodes_estimate;
	fail_estimate = entry.fail_estimate;
	solution_estimate = entry.solution_estimate;
	materialized_nodes = entry.materialized_nodes_count;
	pruned_nodes = entry.pruned_nodes;
}

let uniform_selector _ node =
	Random.int (Array.length node.children)

let sample_rate = function
	| Some (child : 'a node) -> float_of_int child.samples /. (child.fail_estimate +. child.solution_estimate)
	| None -> 0.0

let undersampled_selector (_:'a t) (node : 'a node) : int =
	let n = Array.length node.children in
	if n = 0 then 0
	else
		let rates = Array.init n (fun i -> sample_rate node.children.(i)) in
		let min_rate = Array.fold_left min rates.(0) rates in
		let candidates = List.filter (fun i -> abs_float (rates.(i) -. min_rate) < 1e-8) (List.init n Fun.id) in
	List.nth candidates (Random.int (List.length candidates))


(* Select child with probability proportional to estimated unsampled leaves *)
let probabilistic_undersampled_selector _ (node : 'a node) : int =
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

(** Remaining work metric: nodes_estimate - materialized_nodes.
    None (unmaterialized) = infinity, completed = 0. *)
let greedy_rate child = match child with
  | Some child ->
      if child.isCompleted then Float.infinity  (* Truly infinite — never pick completed *)
      else child.nodes_estimate -. Float.of_int child.materialized_nodes
  | None -> Float.max_float  (* Largest finite float — pick only when no materialized children exist *)

(** Select child with least remaining unmaterialized work.
    Drives branches to completion faster, enabling pruning and memory reclamation. *)
let greedy_completion_selector _ (node : 'a node) : int =
  let n = Array.length node.children in
  if n = 0 then 0
  else
    let rates = Array.init n (fun i -> greedy_rate node.children.(i)) in
    let min_rate = Array.fold_left min rates.(0) rates in
    let candidates = List.filter (fun i -> abs_float (rates.(i) -. min_rate) < 1e-8) (List.init n Fun.id) in
    List.nth candidates (Random.int (List.length candidates))

let rec walk select_child on_solution (node : 'a node) : unit =
	match Lazy.force node.node_view with
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
				node.samples <- Array.fold_left (fun acc (child_opt : 'a node option) -> 
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
					node.pruned_nodes <- Array.fold_left (fun acc (child_opt : 'a node option) -> match child_opt with Some c -> acc + c.pruned_nodes | None -> acc) 0 node.children
				)
			)
	)

type estimates = {
	nodes : float;
	fails : float;
	solutions : float;
	materialized_nodes : int;
  pruned_nodes : int;
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

let create ?(selector=undersampled_selector) ?(on_solution=(fun _ -> ())) (space : 'a Searchspace.t) : 'a t =
	{ root = create_node space; selector; on_solution }

let estimate ?(selector=undersampled_selector) n_trials (space : 'a Searchspace.t) : estimates =
  let est = create ~selector space in
  let root = est.root in
	for _ = 1 to n_trials do
		ignore (walk (selector est) (fun _ -> ()) root)
	done;
	{
		nodes = root.nodes_estimate;
		fails = root.fail_estimate;
		solutions = root.solution_estimate;
		materialized_nodes = root.materialized_nodes;
    pruned_nodes = root.pruned_nodes;
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

let sample n (est : 'a t) : bool =
	let rec loop n =
		if n <= 0 || est.root.isCompleted then ()
		else (
			ignore (walk (est.selector est) est.on_solution est.root);
			loop (n-1)
		)
	in loop n; est.root.isCompleted

let estimates (est : 'a t) : estimates =
	{
		nodes = est.root.nodes_estimate;
		fails = est.root.fail_estimate;
		solutions = est.root.solution_estimate;
		materialized_nodes = est.root.materialized_nodes;
		pruned_nodes = est.root.pruned_nodes;
	}

let is_completed (est : 'a t) : bool = est.root.isCompleted

let analyze_materialized (est : 'a t) : materialized_stats =
	(* Walk the materialized tree and collect depth statistics. Read-only - no new materialization. *)
	let fail_depths = ref [] in
	let sol_depths = ref [] in
	let fork_depths = ref [] in
	let rec walk depth node =
		match Lazy.force node.node_view with
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
    match Lazy.force node.node_view with
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
    match Lazy.force node.node_view with
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

let%expect_test "callback receives solutions across multiple sample() calls" = begin
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

(* Play-around test: simple search space to inspect pruning propagation *)
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

let%expect_test "greedy_completion_selector: step-by-step inspection" = begin
  let num = of_list [1;2;3] in
  let simple_space =
     num |=> (fun x -> 
       num |=> (fun y -> 
        return (x + y)
       )
     ) 
     |?> (fun sum -> sum>4) in 
  let est = create ~selector:greedy_completion_selector simple_space in
  Random.full_init [|42|];
  
  Printf.printf "=== Before sampling ===\n";
  print_tree "" est.root;
  
  for batch = 1 to 8 do
    ignore (sample 1 est);
    Printf.printf "\n=== After %d samples ===\n" batch;
    print_tree "" est.root
  done;
  [%expect{|
    === Before sampling ===
    Fork [samples=0 nodes=1. completed=false materialized=1 pruned=0]
      Child 0: not materialized
      Child 1: not materialized
      Child 2: not materialized

    === After 1 samples ===
    Fork [samples=1 nodes=13. completed=false materialized=3 pruned=0]
      Child 0: not materialized
      Child 1:
        Fork [samples=1 nodes=4. completed=false materialized=2 pruned=0]
          Child 0: not materialized
          Child 1:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 2: not materialized
      Child 2: not materialized

    === After 2 samples ===
    Fork [samples=2 nodes=13. completed=false materialized=4 pruned=0]
      Child 0: not materialized
      Child 1:
        Fork [samples=2 nodes=4. completed=false materialized=3 pruned=0]
          Child 0:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 1:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 2: not materialized
      Child 2: not materialized

    === After 3 samples ===
    Fork [samples=3 nodes=13. completed=false materialized=5 pruned=3]
      Child 0: not materialized
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2: not materialized

    === After 4 samples ===
    Fork [samples=4 nodes=13. completed=false materialized=7 pruned=3]
      Child 0:
        Fork [samples=1 nodes=4. completed=false materialized=2 pruned=0]
          Child 0: not materialized
          Child 1:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 2: not materialized
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2: not materialized

    === After 5 samples ===
    Fork [samples=5 nodes=13. completed=false materialized=8 pruned=3]
      Child 0:
        Fork [samples=2 nodes=4. completed=false materialized=3 pruned=0]
          Child 0:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 1:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 2: not materialized
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2: not materialized

    === After 6 samples ===
    Fork [samples=6 nodes=13. completed=false materialized=9 pruned=6]
      Child 0:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2: not materialized

    === After 7 samples ===
    Fork [samples=7 nodes=13. completed=false materialized=11 pruned=6]
      Child 0:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 1:
        Fork [samples=3 nodes=4. completed=true materialized=4 pruned=3] **PRUNED**
      Child 2:
        Fork [samples=1 nodes=4. completed=false materialized=2 pruned=0]
          Child 0:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
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
          Child 0:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
          Child 1: not materialized
          Child 2:
            Fork [samples=1 nodes=1. completed=true materialized=1 pruned=0] **PRUNED**
    |}]
end

(** ============================================================================
    Phase 1: Memory-Aware Selector Scenario Test
    ============================================================================ **)

(** Memory-aware selector that switches between undersampled and greedy modes.
    When memory is plentiful (below threshold), uses undersampled_selector to spread
    samples across branches. When memory is tight (above threshold), switches to
    greedy_completion_selector to focus on completing branches, enabling pruning.
    
    @param threshold Memory usage threshold. Above this, switches to greedy.
    @param memfree Function that returns the fraction of free memory.
    @return Selector function (estimator -> node -> int) *)
let hard_braking_memory_aware_selector ~threshold ~memory_pressure est (node : 'a node) : int =
  if memory_pressure est > threshold then greedy_completion_selector est node
  else undersampled_selector est node

let%expect_test "hard_braking_memory_aware_selector: scenario - switches between undersampled and greedy" = begin
  Random.full_init [|42|];
  
  (* Create a tree with 3 children, each leading to a deeper subtree *)
  let tree = Searchspace.(
    alt [
      alt [return "A1"; return "A2"];
      alt [return "B1"; return "B2"];
      alt [return "C1"; return "C2"]
    ]
  ) in
  
  (* Mock memory_pressure: return net materialized nodes as pressure *)
  let mock_memory_pressure est = Float.of_int (est.root.materialized_nodes - est.root.pruned_nodes) in
  
  (* Create estimator with hard_braking_memory_aware_selector, threshold at 5 net nodes *)
  let selector = hard_braking_memory_aware_selector ~threshold:5.0 ~memory_pressure:mock_memory_pressure in
  let est = create ~selector tree in
  
  (* Helper to print current mode *)
  let print_mode () =
    let pressure = mock_memory_pressure est in
    if pressure > 5.0 then Printf.printf "Mode: GREEDY (net_nodes=%.0f)\n" pressure
    else Printf.printf "Mode: UNDERSAMPLED (net_nodes=%.0f)\n" pressure
  in
  
  Printf.printf "=== Initial state ===\n";
  print_mode ();
  Printf.printf "Root: samples=%d materialized=%d pruned=%d\n" est.root.samples est.root.materialized_nodes est.root.pruned_nodes;
  
  for batch = 1 to 8 do
    ignore (sample 1 est);
    Printf.printf "\n=== After sample %d ===\n" batch;
    print_mode ();
    Printf.printf "Root: samples=%d materialized=%d pruned=%d\n" est.root.samples est.root.materialized_nodes est.root.pruned_nodes;
    
    (* Print child summary *)
    match Lazy.force est.root.node_view with
    | Fork _ ->
        for i = 0 to Array.length est.root.children - 1 do
          match est.root.children.(i) with
          | Some c ->
              Printf.printf "  Child %d: samples=%d completed=%b materialized=%d\n" 
                i c.samples c.isCompleted c.materialized_nodes
          | None ->
              Printf.printf "  Child %d: not materialized\n" i
        done
    | _ -> ()
  done;
  
  [%expect{|
    === Initial state ===
    Mode: UNDERSAMPLED (net_nodes=1)
    Root: samples=0 materialized=1 pruned=0

    === After sample 1 ===
    Mode: UNDERSAMPLED (net_nodes=3)
    Root: samples=1 materialized=3 pruned=0
      Child 0: not materialized
      Child 1: samples=1 completed=false materialized=2
      Child 2: not materialized

    === After sample 2 ===
    Mode: UNDERSAMPLED (net_nodes=5)
    Root: samples=2 materialized=5 pruned=0
      Child 0: not materialized
      Child 1: samples=1 completed=false materialized=2
      Child 2: samples=1 completed=false materialized=2

    === After sample 3 ===
    Mode: GREEDY (net_nodes=7)
    Root: samples=3 materialized=7 pruned=0
      Child 0: samples=1 completed=false materialized=2
      Child 1: samples=1 completed=false materialized=2
      Child 2: samples=1 completed=false materialized=2

    === After sample 4 ===
    Mode: GREEDY (net_nodes=6)
    Root: samples=4 materialized=8 pruned=2
      Child 0: samples=1 completed=false materialized=2
      Child 1: samples=2 completed=true materialized=3
      Child 2: samples=1 completed=false materialized=2

    === After sample 5 ===
    Mode: UNDERSAMPLED (net_nodes=5)
    Root: samples=5 materialized=9 pruned=4
      Child 0: samples=2 completed=true materialized=3
      Child 1: samples=2 completed=true materialized=3
      Child 2: samples=1 completed=false materialized=2

    === After sample 6 ===
    Mode: UNDERSAMPLED (net_nodes=1)
    Root: samples=6 materialized=10 pruned=9

    === After sample 7 ===
    Mode: UNDERSAMPLED (net_nodes=1)
    Root: samples=6 materialized=10 pruned=9

    === After sample 8 ===
    Mode: UNDERSAMPLED (net_nodes=1)
    Root: samples=6 materialized=10 pruned=9
    |}]
end

(** ============================================================================
    Gradual Braking Selector Implementation
    ============================================================================ **)

(** Statistics tracking which strategy was used by the gradual braking selector. *)
type gradual_braking_stats = {
  total_calls : int;
  undersampled_count : int;
  greedy_count : int;
}

(** Gradual braking selector that eases off undersampled behavior as a measured value approaches a threshold.
    Uses the formula U + (C mod T) < T to provide linear decay blending from 100% undersampled
    at U=0 to 0% undersampled at U=T. Prevents the "freight train" overshoot problem by starting
    braking immediately rather than waiting for pressure to hit a hard threshold.
    
    The selector is unit-agnostic: [memory_pressure] returns any numeric value representing pressure,
    and [threshold] must be in the same units. This allows plugging in different measurement
    sources (RSS from /proc, heap words, system memory, etc.) without conversion logic in the selector.
    
    Returns a tuple of (selector_function, stats_accessor). The stats accessor provides
    cumulative counts of which strategy was used across all calls.
    
    @param threshold Pressure value at which undersampled probability reaches 0% (default 8000.0).
    @param memory_pressure Function that receives the estimator and returns current pressure value.
    @return Tuple of (selector function, stats accessor) *)
let gradual_braking_memory_aware_selector ~threshold ~memory_pressure
  : ('a child_selector * (unit -> gradual_braking_stats)) =
  let total_calls = ref 0 in
  let undersampled_count = ref 0 in
  let greedy_count = ref 0 in
  let last_measure = ref 0.0 in
  (* Ref to current estimator - set by create when selector is stored *)
  let selector est (node : 'a node) : int =
    incr total_calls;
    let u = memory_pressure est in
    last_measure := u;
    (* Random float [0,1) compared against U/T ratio gives P(undersampled) = (T-U)/T.
       Clean, unit-agnostic — no floor hacks or float modulo. Same cost as prime mod
       (just a division), but avoids all the int<->float conversion mess. *)
    let ratio = u /. threshold in
    if Random.float 1.0 >= ratio then (
      incr undersampled_count;
      undersampled_selector est node
    ) else (
      incr greedy_count;
      greedy_completion_selector est node
    )
  in
  let get_stats () : gradual_braking_stats = 
    let r = {
      total_calls = !total_calls;
      undersampled_count = !undersampled_count;
      greedy_count = !greedy_count;
    } in begin
      undersampled_count := 0; greedy_count := 0;
      r
    end
  in (selector, get_stats)

(** ============================================================================
    Gradual Braking Selector Tests
    ============================================================================ **)

(* Build a search space large enough to sample many times without exhausting. *)
(* int_range creates nested binary choices, but we just need enough depth for 100+ samples *)
let large_tree () : (int * int * int) Searchspace.t =
  let* x = Searchspace.int_range 1 5 in
  let* y = Searchspace.int_range 1 5 in
  let* z = Searchspace.int_range 1 5 in
  return (x, y, z)

let%expect_test "selector: linear decay across U/T ratios" = begin
  Random.full_init [|42|];
  let t = 100.0 in
  Printf.printf "U/T | total | undersampled | greedy | %%undersampled\n";
  Printf.printf "----+-------+--------------+--------+-------------\n";
  for i = 0 to 4 do
    let u = Float.of_int ((i * int_of_float t) / 4) in
    let tree = large_tree () in
    let (selector, get_stats) = gradual_braking_memory_aware_selector 
      ~threshold:t
      ~memory_pressure:(fun _ -> u) in
    let est = create ~selector tree in
    ignore (sample 100 est);
    let s = get_stats () in
    let pct = Float.of_int s.undersampled_count /. Float.of_int s.total_calls *. 100.0 in
    Printf.printf "%3.0f/%4d | %5d | %12d | %6d | %.1f%%\n" 
      u (int_of_float t) s.total_calls s.undersampled_count s.greedy_count pct
  done;
  [%expect{|
    U/T | total | undersampled | greedy | %undersampled
    ----+-------+--------------+--------+-------------
      0/ 100 |   820 |          820 |      0 | 100.0%
     25/ 100 |   807 |          610 |    197 | 75.6%
     50/ 100 |   806 |          385 |    421 | 47.8%
     75/ 100 |   832 |          212 |    620 | 25.5%
    100/ 100 |   919 |            0 |    919 | 0.0%
    |}]
end

let%expect_test "selector: stats are independent per selector" = begin
  Random.full_init [|42|];
  let t = 10.0 in
  let tree_a = large_tree () in
  let tree_b = large_tree () in
  let sel_a, get_stats_a = gradual_braking_memory_aware_selector 
    ~threshold:t
    ~memory_pressure:(fun _ -> 0.0) in
  let sel_b, get_stats_b = gradual_braking_memory_aware_selector 
    ~threshold:t
    ~memory_pressure:(fun _ -> t) in
  let est_a = create ~selector:sel_a tree_a in
  let est_b = create ~selector:sel_b tree_b in
  ignore (sample 20 est_a);
  ignore (sample 20 est_b);
  let sa = get_stats_a () in
  let sb = get_stats_b () in
  Printf.printf "A: total=%d undersampled=%d greedy=%d\n" 
    sa.total_calls sa.undersampled_count sa.greedy_count;
  Printf.printf "B: total=%d undersampled=%d greedy=%d\n" 
    sb.total_calls sb.undersampled_count sb.greedy_count;
  [%expect{|
    A: total=128 undersampled=128 greedy=0
    B: total=149 undersampled=0 greedy=149
    |}]
end

(** ============================================================================
    State Serialization - Decision Path Encoding
    ============================================================================ **)

(* Simple line-based format for serialization:
   Line 1: version N
   Lines 2+: path|num_choices|samples|nodes_estimate|fail_estimate|solution_estimate|materialized_nodes_count|pruned_nodes|is_completed
   Path format: "0/3,1/2" means decision 0 of 3, then decision 1 of 2
*)

let rec collect_entries (node : 'a node) (path : decision_path) : node_entry Seq.t =
  let num_choices = match Lazy.force node.node_view with
    | Fork choices -> List.length choices
    | _ -> 0
  in
  let entry = {
    path;
    num_choices;
    samples = node.samples;
    nodes_estimate = node.nodes_estimate;
    fail_estimate = node.fail_estimate;
    solution_estimate = node.solution_estimate;
    materialized_nodes_count = node.materialized_nodes;
    pruned_nodes = node.pruned_nodes;
    is_completed = node.isCompleted;
  } in
  Seq.cons entry (
    if node.isCompleted then Seq.empty (* pruned nodes have no children to visit *)
    else
      Array.to_seqi node.children |> Seq.filter_map (fun (i, child_opt) ->
        match child_opt with
        | Some child -> Some (collect_entries child (path @ [{chosen=i; choices=num_choices}]))
        | None -> None
      ) |> Seq.concat
  )

let decision_to_string d = string_of_int d.chosen ^ "/" ^ string_of_int d.choices

let path_to_string path = String.concat "," (List.map decision_to_string path)

let entry_to_line (entry : node_entry) : string =
  String.concat "|" [
    path_to_string entry.path;
    string_of_int entry.num_choices;
    string_of_int entry.samples;
    string_of_float entry.nodes_estimate;
    string_of_float entry.fail_estimate;
    string_of_float entry.solution_estimate;
    string_of_int entry.materialized_nodes_count;
    string_of_int entry.pruned_nodes;
    string_of_bool entry.is_completed;
  ]

let save_state (filename : string) (est : 'a t) =
  let oc = open_out filename in
  Printf.fprintf oc "version 1\n";
  collect_entries est.root [] |> Seq.iter (fun entry -> Printf.fprintf oc "%s\n" (entry_to_line entry));
  close_out oc

(* Parse a decision from string "chosen/choices" *)
let parse_decision s =
  let parts = String.split_on_char '/' s in
  { chosen = int_of_string (List.hd parts); choices = int_of_string (List.nth parts 1) }

(* Parse a path from string "0/3,1/2" *)
let parse_path s =
  if s = "" then []
  else List.map parse_decision (String.split_on_char ',' s)

(* Parse a line into a node_entry *)
let parse_line (line : string) : node_entry =
  let parts = String.split_on_char '|' line in
  if List.length parts <> 9 then
    failwith ("Invalid entry format (expected 9 fields, got " ^ string_of_int (List.length parts) ^ ")");
  {
    path = parse_path (List.hd parts);
    num_choices = int_of_string (List.nth parts 1);
    samples = int_of_string (List.nth parts 2);
    nodes_estimate = float_of_string (List.nth parts 3);
    fail_estimate = float_of_string (List.nth parts 4);
    solution_estimate = float_of_string (List.nth parts 5);
    materialized_nodes_count = int_of_string (List.nth parts 6);
    pruned_nodes = int_of_string (List.nth parts 7);
    is_completed = match List.nth parts 8 with "true" -> true | _ -> false;
  }

(* Replay a decision path and apply an entry's stats to the corresponding node.
  Each entry in the file corresponds to exactly one node identified by its path.
  Since entries are in preorder, parent nodes already exist when we process children. *)
let replay_path (_space : 'a Searchspace.t) (root : 'a node) (entry : node_entry) : unit =
  (* Apply entry stats to an existing node *)
  let apply_stats (node : 'a node) (entry : node_entry) : unit =
    node.samples <- entry.samples;
    node.nodes_estimate <- entry.nodes_estimate;
    node.fail_estimate <- entry.fail_estimate;
    node.solution_estimate <- entry.solution_estimate;
    node.materialized_nodes <- entry.materialized_nodes_count;
    node.pruned_nodes <- entry.pruned_nodes;
    node.isCompleted <- entry.is_completed
  in
  match entry.path with
  | [] ->
      (* Root entry — apply stats to the root node *)
      apply_stats root entry
  | { chosen = _first; choices = _nc } :: _rest ->
      (* Navigate the path and create the target node at the end.
         Child views are constructed lazily from parent's lazy view — nothing is forced. *)
      let rec navigate (node : 'a node) (path : decision_path) : unit =
        match path with
        | [] -> ()  (* We've reached the target — already created by previous step *)
        | { chosen = c; choices = _nc } :: rest' ->
            let child_opt : 'a node option = node.children.(c) in
            match (child_opt, rest') with
            | (Some (child : 'a node)), _ ->
                navigate child rest'
            | (None, []) ->
                (* Create target node with a lazy view derived from parent's lazy view.
                   Nothing is forced — the child's inspect will only happen when this node
                   is actually inspected during sampling. *)
                let child_view : 'a Searchspace.node_view Lazy.t = lazy (
                  match Lazy.force node.node_view with
                  | Fork choices -> inspect (List.nth choices c)
                  | _ -> Fail
                ) in
                let new_node : 'a node = create_node_lazy child_view entry in
                node.children.(c) <- Some new_node
            | (None, _ :: _) ->
                (* Intermediate node missing — shouldn't happen in preorder *)
                failwith "Intermediate node missing during replay (preorder violation)"
      in
      navigate root entry.path

(* Load state from file and reconstruct estimator *)
let load_state ?(selector = undersampled_selector) ?(on_solution = (fun _ -> ())) (space : 'a Searchspace.t) (filename : string) : 'a t =
  let ic = open_in filename in
  (* Read version header *)
  let first_line = try Some (input_line ic) with End_of_file -> None in
  match first_line with
  | None -> close_in ic; failwith "Invalid file format: empty file"
  | Some first_line ->
      let version_parts = String.split_on_char ' ' first_line in
      match version_parts with
      | ["version"; "1"] ->
          (* Read the first entry — it must be the root (path = []) *)
          let root_entry_line = try Some (input_line ic) with End_of_file -> None in
          let root_entry = match root_entry_line with
            | None -> close_in ic; failwith "Invalid file format: no root entry"
            | Some root_line -> parse_line root_line
          in
          (* Create root node with lazy view and correct children size from entry — nothing forced. *)
          let root_view : 'a Searchspace.node_view Lazy.t = lazy (inspect space) in
          let root : 'a node = create_node_lazy root_view root_entry in
          
          (* Stream remaining entries one at a time — file is already in DFS pre-order, so parents come before children *)
          let rec loop () = try
            let line = input_line ic in
            let entry = parse_line line in
            replay_path space root entry;  (* applies stats to the correct node *)
            loop ()
          with End_of_file -> close_in ic
          in
          loop ();
          
          { root; selector; on_solution }
      | ["version"; v] -> close_in ic; failwith ("Unsupported version: " ^ v)
      | _ -> close_in ic; failwith "Invalid file format: expected 'version N' as first line"

(** ============================================================================
    POINT ON THE HORIZON: Round-trip serialization test
    This is the proof that serialization/deserialization works correctly.
    ============================================================================ **)

let show_stats est =
  let stats = estimates est in
  Printf.sprintf "nodes=%.0f fails=%.0f sols=%.0f mat=%d pru=%d completed=%b" 
    stats.nodes stats.fails stats.solutions stats.materialized_nodes stats.pruned_nodes (is_completed est)   

let%expect_test "roundtrip: serialize/deserialize/resume produces same result as single run" = begin
  (* Larger search space so partial sampling leaves an incomplete tree *)
  let num = Searchspace.of_list [1;2;3;4;5] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  (* Run A: sample to completion in one shot *)
  let est_a = create space in
  ignore (sample 1000 est_a);
  Printf.printf "Run A (single shot): %s\n" (show_stats est_a);

  (* Run B: sample partway, serialize to file, deserialize, resume *)
  let est_b = create space in
  ignore (sample 5 est_b);
  Printf.printf "Run B partial:       %s\n" (show_stats est_b);
  save_state "test_save.sexp" est_b;
  let est_b_resumed = load_state space "test_save.sexp" in
  Printf.printf "Run B after load:    %s\n" (show_stats est_b_resumed);
  ignore (sample 1000 est_b_resumed);
  Printf.printf "Run B (roundtrip):   %s\n" (show_stats est_b_resumed);

  (* Both should match — same estimates, same completion state *)
  [%expect{|
    Run A (single shot): nodes=36 fails=5 sols=15 mat=36 pru=35 completed=true
    Run B partial:       nodes=36 fails=8 sols=12 mat=15 pru=0 completed=false
    Run B after load:    nodes=36 fails=8 sols=12 mat=15 pru=0 completed=false
    Run B (roundtrip):   nodes=36 fails=5 sols=15 mat=36 pru=35 completed=true
    |}]
end

(** ============================================================================
    Edge Cases for Serialization/Deserialization
    ============================================================================ **)

let%expect_test "edge case: single-node tree (no forks)" = begin
  (* A search space with zero decision points — just a Result *)
  let space = return 42 in

  (* Sample to completion (instant, since it's a single node) *)
  let est = create space in
  ignore (sample 10 est);

  (* Serialize and deserialize *)
  save_state "/tmp/test_single.sexp" est;
  let est2 = load_state space "/tmp/test_single.sexp" in

  (* Verify: single node, completed *)
  let r = estimates est2 in
  Printf.printf "nodes=%.0f fails=%.0f sols=%.0f mat=%d completed=%b\n"
    r.nodes r.fails r.solutions r.materialized_nodes (is_completed est2);

  [%expect{|
    nodes=1 fails=0 sols=1 mat=1 completed=true
  |}]
end

let%expect_test "edge case: deep tree (5 levels)" = begin
  (* Deep nesting: pick from [1..3] at each of 5 levels *)
  let num = of_list [1;2;3] in
  let space =
    num |=> (fun a ->
      num |=> (fun b ->
        num |=> (fun c ->
          num |=> (fun d ->
            num |=> (fun e ->
              return (a, b, c, d, e)
            )
          )
        )
      )
    ) in

  (* Sample a few times — won't complete the full tree *)
  let est = create space in
  ignore (sample 20 est);

  save_state "/tmp/test_deep.sexp" est;
  let est2 = load_state space "/tmp/test_deep.sexp" in

  (* Verify structure survived *)
  let r = estimates est2 in
  Printf.printf "nodes=%.0f fails=%.0f sols=%.0f mat=%d completed=%b\n"
    r.nodes r.fails r.solutions r.materialized_nodes (is_completed est2);

  [%expect{| nodes=364 fails=0 sols=243 mat=73 completed=false |}]
end

let%expect_test "edge case: wide tree (many choices at root)" = begin
  (* Wide branching: pick from [1..20] *)
  let space = of_list (List.init 20 (fun i -> i + 1)) in

  let est = create space in
  ignore (sample 50 est);

  save_state "/tmp/test_wide.sexp" est;
  let est2 = load_state space "/tmp/test_wide.sexp" in

  let r = estimates est2 in
  Printf.printf "nodes=%.0f fails=%.0f sols=%.0f mat=%d completed=%b\n"
    r.nodes r.fails r.solutions r.materialized_nodes (is_completed est2);

  [%expect{| nodes=21 fails=0 sols=20 mat=21 completed=true |}]
end

let%expect_test "edge case: partial tree with unmaterialized children" = begin
  (* Two-level tree, sample only a few branches so many children are None *)
  let num = of_list [1;2;3;4;5] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  let est = create space in
  (* Sample just a few times — most children will be unmaterialized *)
  ignore (sample 3 est);

  save_state "/tmp/test_partial.sexp" est;
  let est2 = load_state space "/tmp/test_partial.sexp" in

  (* After loading, the tree should have the same structure.
     Sampling more should continue from where we left off. *)
  ignore (sample 50 est2);

  let r = estimates est2 in
  Printf.printf "nodes=%.0f fails=%.0f sols=%.0f mat=%d completed=%b\n"
    r.nodes r.fails r.solutions r.materialized_nodes (is_completed est2);

  [%expect{| nodes=36 fails=5 sols=15 mat=36 completed=true |}]
end

let%expect_test "edge case: resume after load accumulates samples correctly" = begin
  (* Verify that sampling before save + sampling after resume = same as continuous *)
  let num = of_list [1;2;3] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  (* Continuous run: sample 100 straight through *)
  let est_cont = create space in
  ignore (sample 100 est_cont);
  let r_cont = estimates est_cont in

  (* Split run: sample 30, save, load, sample 70 *)
  let est_split = create space in
  ignore (sample 30 est_split);
  save_state "/tmp/test_resume.sexp" est_split;
  let est_resumed = load_state space "/tmp/test_resume.sexp" in
  ignore (sample 70 est_resumed);
  let r_split = estimates est_resumed in

  Printf.printf "continuous: nodes=%.0f fails=%.0f sols=%.0f mat=%d\n"
    r_cont.nodes r_cont.fails r_cont.solutions r_cont.materialized_nodes;
  Printf.printf "split:      nodes=%.0f fails=%.0f sols=%.0f mat=%d\n"
    r_split.nodes r_split.fails r_split.solutions r_split.materialized_nodes;

  (* Both should be very close — same total samples, same tree *)
  [%expect{|
    continuous: nodes=16 fails=3 sols=6 mat=16
    split:      nodes=16 fails=3 sols=6 mat=16
    |}]
end

let%expect_test "edge case: multiple round-trips" = begin
  (* Save → load → sample → save → load, verify consistency *)
  let num = of_list [1;2;3] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  let est = create space in
  ignore (sample 20 est);
  save_state "/tmp/test_multi1.sexp" est;

  let est2 = load_state space "/tmp/test_multi1.sexp" in
  ignore (sample 30 est2);
  save_state "/tmp/test_multi2.sexp" est2;

  let est3 = load_state space "/tmp/test_multi2.sexp" in
  ignore (sample 50 est3);

  let r = estimates est3 in
  Printf.printf "nodes=%.0f fails=%.0f sols=%.0f mat=%d completed=%b\n"
    r.nodes r.fails r.solutions r.materialized_nodes (is_completed est3);

  [%expect{| nodes=16 fails=3 sols=6 mat=16 completed=true |}]
end

let%expect_test "edge case: completed tree survives round-trip" = begin
  (* Sample to full completion, then serialize *)
  let num = of_list [1;2;3] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  let est = create space in
  ignore (sample 1000 est); (* should complete *)

  save_state "/tmp/test_completed.sexp" est;
  let est2 = load_state space "/tmp/test_completed.sexp" in

  (* Should already be completed, no new sampling needed *)
  Printf.printf "completed=%b nodes=%.0f fails=%.0f sols=%.0f mat=%d\n"
    (is_completed est2)
    (estimates est2).nodes
    (estimates est2).fails
    (estimates est2).solutions
    (estimates est2).materialized_nodes;

  [%expect{| completed=true nodes=16 fails=3 sols=6 mat=16 |}]
end

let%expect_test "edge case: pruned nodes survive round-trip" = begin
  (* Pruning happens when subtrees complete. Verify pruned_nodes count survives. *)
  let num = of_list [1;2;3] in
  let space =
    num |=> (fun x ->
      int_range 1 x |=> (fun y ->
        return (x, y)
      )
    ) in

  let est = create space in
  ignore (sample 1000 est); (* complete → prune *)

  save_state "/tmp/test_pruned.sexp" est;
  let est2 = load_state space "/tmp/test_pruned.sexp" in

  let r1 = estimates est in
  let r2 = estimates est2 in
  Printf.printf "original: pruned=%d mat=%d\n" r1.pruned_nodes r1.materialized_nodes;
  Printf.printf "roundtrip: pruned=%d mat=%d\n" r2.pruned_nodes r2.materialized_nodes;

  [%expect{|
    original: pruned=15 mat=16
    roundtrip: pruned=15 mat=16
    |}]
end

let%expect_test "edge case: empty file should fail" = begin
  let () =
    let oc = open_out "/tmp/test_empty.sexp" in
    close_out oc
  in
  let space = return () in
  try
    ignore (load_state space "/tmp/test_empty.sexp");
    Printf.printf "ERROR: should have failed\n"
  with Failure msg ->
    Printf.printf "Correctly rejected empty file: %s\n" msg;

  [%expect{|
    Correctly rejected empty file: Invalid file format: empty file
  |}]
end

let%expect_test "edge case: wrong version should fail" = begin
  let () =
    let oc = open_out "/tmp/test_badver.sexp" in
    Printf.fprintf oc "version 2\n";
    close_out oc
  in
  let space = return () in
  try
    ignore (load_state space "/tmp/test_badver.sexp");
    Printf.printf "ERROR: should have failed\n"
  with Failure msg ->
    Printf.printf "Correctly rejected bad version: %s\n" msg;

  [%expect{|
    Correctly rejected bad version: Unsupported version: 2
  |}]
end

let%expect_test "edge case: malformed line should fail" = begin
  let () =
    let oc = open_out "/tmp/test_badline.sexp" in
    Printf.fprintf oc "version 1\n";
    Printf.fprintf oc "this is not a valid entry\n";
    close_out oc
  in
  let num = of_list [1;2] in
  let space = num |=> (fun x -> return x) in
  try
    ignore (load_state space "/tmp/test_badline.sexp");
    Printf.printf "ERROR: should have failed\n"
  with Failure msg ->
    Printf.printf "Correctly rejected malformed line: %s\n" msg;

  [%expect{| Correctly rejected malformed line: Invalid entry format (expected 9 fields, got 1) |}]
end

let%expect_test "lazy views on load_state" = begin
  (* Counter that increments each time inspect is called *)
  let inspect_count = ref 0 in

  (* Space with 20 branches — each child's inspect increments the counter *)
  let nums = int_range 1 20 |=> fun x -> begin
      inspect_count := !inspect_count + 1;
      return x
  end in

  let space = (
    let* x = nums in
    let* y = nums in
    return (x+y)
  ) in 

  (* Sample a few paths — creates some materialized nodes *)
  let est = create space in
  ignore (sample 20 est);
  Printf.printf "After sampling: %d inspections, mat=%d\n" !inspect_count (estimates est).materialized_nodes;

  (* Save state *)
  save_state "/tmp/test_lazy.sexp" est;

  inspect_count := 0;
  (* Load state — check counter immediately, before any new sampling *)
  let est = load_state space "/tmp/test_lazy.sexp" in
  Printf.printf "After load (before resume): inspections %d, mat=%d\n" !inspect_count (estimates est).materialized_nodes;

  (* Resume sampling — only visits some branches *)
  ignore (sample 10 est);
  Printf.printf "After resume: %d inspections, mat=%d\n" !inspect_count (estimates est).materialized_nodes;

  [%expect{|
    After sampling: 30 inspections, mat=50
    After load (before resume): inspections 0, mat=50
    After resume: 16 inspections, mat=77
    |}]
end
