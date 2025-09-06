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
	; [%expect{|Always first: [0/2, 0/2] => Found: 1 + 1 = 2
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
		mutable children : 'a node option array;        (* Children indexed by decision number; only some may be materialized *)
		mutable samples : int;                          (* Number of samples passing through this node *)
		mutable nodes_estimate : float;                 (* Current best estimate for subtree size *)
		mutable fail_estimate : float;                  (* Final estimate for failures in this subtree *)
		mutable solution_estimate : float;              (* Final estimate for solutions in this subtree *)
}


let child_average (children : 'a node option array) (f : 'a node -> float) : float =
	let materialized = Array.to_list children |> List.filter_map (fun c -> c) in
	let avg =
		match materialized with
		| [] -> 1.0
		| xs -> List.fold_left ( +. ) 0. (List.map f xs) /. float_of_int (List.length xs)
	in
	avg

let children_estimate (children : 'a node option array) (f : 'a node -> float) : float =
	float_of_int (Array.length children) *. child_average children f

let num_choices node_view = match node_view with
	| Fork choices -> List.length choices
	| _ -> 0

let create_node (space : 'a Searchspace.t) : 'a node =
	let node_view = inspect space in
	let (nodes_estimate, fail_estimate, solution_estimate) = match node_view with
		| Result _ -> (1.0, 0.0, 1.0)
		| Fail    -> (1.0, 1.0, 0.0)
		| Fork _  -> (1.0, 0.0, 0.0) (* initial values for forks, will be updated by sampling *)
	in {
		node_view;
		children = Array.make (num_choices node_view) None;
		samples = 0;
		nodes_estimate;
		fail_estimate;
		solution_estimate;
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


let weighted_selector (node : 'a node) : int =
	let n = Array.length node.children in
	if n = 0 then 0
	else
		let materialized = Array.to_list node.children |> List.filter_map (fun c -> c) in
		let avg =
			match materialized with
			| [] -> 1.0
			| xs -> List.fold_left ( +. ) 0. (List.map (fun c -> c.nodes_estimate) xs) /. float_of_int (List.length xs)
		in
		let weights = Array.init n (fun i ->
			match node.children.(i) with
			| Some child -> max child.nodes_estimate 1.0
			| None -> avg
		) in
		let total = Array.fold_left ( +. ) 0.0 weights in
		let r = Random.float total in
		let rec pick i acc =
			if i >= n then n - 1
			else if acc +. weights.(i) >= r then i
			else pick (i+1) (acc +. weights.(i))
		in pick 0 0.0

let rec walk select_child (node : 'a node) : unit =
	node.samples <- node.samples + 1;
	match node.node_view with
	| Fail | Result _ -> ()
	| Fork choices ->
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
			walk select_child child_node;
			node.nodes_estimate <- 1. +. children_estimate node.children (fun child -> child.nodes_estimate);
			node.fail_estimate <- children_estimate node.children (fun child -> child.fail_estimate);
			node.solution_estimate <- children_estimate node.children (fun child -> child.solution_estimate);
		)


type estimates = {
	nodes : float;
	fails : float;
	solutions : float;
	materialized_nodes : int;
}

let rec count_materialized_nodes (node : 'a node) : int =
	match node.node_view with
	| Fork _ ->
		1 + Array.fold_left (fun acc child_opt ->
			match child_opt with
			| Some child -> acc + count_materialized_nodes child
			| None -> acc
		) 0 node.children
	| _ -> 1

let estimate ?(selector=undersampled_selector) n_trials (space : 'a Searchspace.t) : estimates =
	let root = create_node space in
	for _ = 1 to n_trials do
		walk selector root
	done;
	{
		nodes = root.nodes_estimate;
		fails = root.fail_estimate;
		solutions = root.solution_estimate;
		materialized_nodes = count_materialized_nodes root;
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
     number of nodes: 19015
     number of fails: 7992
     number of solutions: 1516

   Sample run 2000:
   Estimated values balanced trees:
     materialized nodes: 8273
     number of nodes: 18967
     number of fails: 8092
     number of solutions: 1392

   Sample run 3000:
   Estimated values balanced trees:
     materialized nodes: 10377
     number of nodes: 18077
     number of fails: 7666
     number of solutions: 1373

   Sample run 4000:
   Estimated values balanced trees:
     materialized nodes: 12221
     number of nodes: 18287
     number of fails: 7830
     number of solutions: 1314

   Sample run 5000:
   Estimated values balanced trees:
     materialized nodes: 13595
     number of nodes: 17327
     number of fails: 7350
     number of solutions: 1314
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
     materialized nodes: 2100
     number of nodes: 2203
     number of fails: 942
     number of solutions: 160

   Sample run 2000:
   Estimated values (unbalanced trees):
     materialized nodes: 4099
     number of nodes: 4199
     number of fails: 1800
     number of solutions: 300

   Sample run 3000:
   Estimated values (unbalanced trees):
     materialized nodes: 6097
     number of nodes: 6195
     number of fails: 2660
     number of solutions: 438

   Sample run 4000:
   Estimated values (unbalanced trees):
     materialized nodes: 8096
     number of nodes: 8193
     number of fails: 3511
     number of solutions: 586

   Sample run 5000:
   Estimated values (unbalanced trees):
     materialized nodes: 10093
     number of nodes: 10187
     number of fails: 4370
     number of solutions: 724
   |}]

(** Incremental estimator API implementation *)
type 'a t = {
	root : 'a node;
	selector : 'a child_selector;
}

let create ?(selector=undersampled_selector) (space : 'a Searchspace.t) : 'a t =
	{ root = create_node space; selector }

let sample n (est : 'a t) : unit =
	for _ = 1 to n do
		walk est.selector est.root
	done

let estimates (est : 'a t) : estimates =
	{
		nodes = est.root.nodes_estimate;
		fails = est.root.fail_estimate;
		solutions = est.root.solution_estimate;
		materialized_nodes = count_materialized_nodes est.root;
	}

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
  for samplers = 1 to 5 do
    let samples = 1000 * samplers in
    sample 1000 est;
    Printf.printf "Sample run %d:\n" samples;
    let estimates = estimates est in
    Printf.printf "Estimated values (incremental):\n";
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
    Estimated values (incremental):
      materialized nodes: 2100
      number of nodes: 2203
      number of fails: 942
      number of solutions: 160

    Sample run 2000:
    Estimated values (incremental):
      materialized nodes: 4100
      number of nodes: 4203
      number of fails: 1800
      number of solutions: 302

    Sample run 3000:
    Estimated values (incremental):
      materialized nodes: 6097
      number of nodes: 6195
      number of fails: 2654
      number of solutions: 444

    Sample run 4000:
    Estimated values (incremental):
      materialized nodes: 8096
      number of nodes: 8193
      number of fails: 3513
      number of solutions: 584

    Sample run 5000:
    Estimated values (incremental):
      materialized nodes: 10095
      number of nodes: 10199
      number of fails: 4384
      number of solutions: 716
    |}]
