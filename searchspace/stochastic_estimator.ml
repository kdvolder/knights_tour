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
	nodes: int;
	forks: int;
	solutions: int;
	failures: int;
}

let rec calculate_stats space = inspect space |> function
	| Result _ -> {nodes=1; forks=0; solutions=1; failures=0}
	| Fail -> {nodes=1; forks=0; solutions=0; failures=1}
	| Fork choices ->
			let children_stats = List.map calculate_stats choices in
			let nodes = 1 + List.fold_left (fun acc s -> acc + s.nodes) 0 children_stats in
			let forks = 1 + List.fold_left (fun acc s -> acc + s.forks) 0 children_stats in
			let solutions = List.fold_left (fun acc s -> acc + s.solutions) 0 children_stats in
			let failures = List.fold_left (fun acc s -> acc + s.failures) 0 children_stats in
			{nodes; forks; solutions; failures}

let sums_div7 =
	let* n1 = int_range 1 4 in
	let* n2 = int_range 1 5 in
	let sum = n1 + n2 in
	if sum mod 7 = 0 then return (Printf.sprintf "%d + %d = %d" n1 n2 sum)
	else empty

let%expect_test "calculate_stats" =
	let stats = calculate_stats sums_div7 in
		Printf.printf "nodes: %d, forks: %d, solutions: %d, failures: %d\n" stats.nodes stats.forks stats.solutions stats.failures;
		[%expect{| nodes: 49, forks: 24, solutions: 3, failures: 22 |}]



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

let rng = Random.int
let rec walk (node : 'a node) : unit =
	node.samples <- node.samples + 1;
	match node.node_view with
	| Fail | Result _ -> ()
	| Fork choices ->
		let num_choices = Array.length node.children in
		if num_choices > 0 then (
			let chosen = rng num_choices in
			let child_node = match node.children.(chosen) with
			  | Some child -> child
			  | None ->
				  let c = create_node (List.nth choices chosen) in
				  node.children.(chosen) <- Some c;
				  c
			in
			walk child_node;
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

let estimate n_trials (space : 'a Searchspace.t) : estimates =
	let root = create_node space in
	for _ = 1 to n_trials do
		walk root
	done;
	{
		nodes = root.nodes_estimate;
		fails = root.fail_estimate;
		solutions = root.solution_estimate;
		materialized_nodes = count_materialized_nodes root;
	}

let%expect_test "estimate number of nodes" =
	let true_values = calculate_stats sums_div7 in
	Printf.printf "True values\n";
	Printf.printf "  number of nodes: %d\n" true_values.nodes;
	Printf.printf "  number of fails: %d\n" true_values.failures;
	Printf.printf "  number of solutions: %d\n" true_values.solutions;
	Printf.printf "\n";
	let estimates = estimate 1000 sums_div7 in
	Printf.printf "Estimated\n";
	Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
	Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
	Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
	Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
	[%expect{|
   Estimated
     number of nodes: 49
     number of fails: 22
     number of solutions: 3
   |}]

(* let rec left_heavy_range start stop =
	if start > stop then empty
	else
		left_heavy_range start (stop - 1) ++ return stop *)

(* let rec list_range start stop =
	if start > stop then []
	else
		start::list_range (start+1) stop *)

(* let rec balanced_range start stop =
	if start > stop then empty
	else if start = stop then return start
	else if start = stop - 1 then return start ++ return stop
	else let mid = (start + stop) / 2 in
		balanced_range start (mid - 1) ++ balanced_range mid stop *)
	
let%expect_test "undersampling larger searchspace" =
	let open Searchspace in
	(* let int_range = balanced_range in *)
	let left_heavy_space = (
		let* n1 = int_range 1 100 in
		let* n2 = int_range 1 100 in
		let sum = return (n1 + n2) in 
		sum |?> (fun x -> x mod 7 = 0)
	) in
		let true_values = calculate_stats left_heavy_space in
		Printf.printf "True values\n";
		Printf.printf "  number of nodes: %d\n" true_values.nodes;
		Printf.printf "  number of fails: %d\n" true_values.failures;
		Printf.printf "  number of solutions: %d\n" true_values.solutions;
		Printf.printf "\n";
		for samplers = 1 to 10 do
			let samples = 5000 * samplers in
			Printf.printf "Sample run %d:\n" samples;
			let estimates = estimate samples left_heavy_space in
			Printf.printf "Estimated values (left-heavy):\n";
			Printf.printf "  materialized nodes: %d\n" estimates.materialized_nodes;
			Printf.printf "  number of nodes: %d\n" (int_of_float (estimates.nodes +. 0.5));
			Printf.printf "  number of fails: %d\n" (int_of_float (estimates.fails +. 0.5));
			Printf.printf "  number of solutions: %d\n" (int_of_float (estimates.solutions +. 0.5));
			Printf.printf "\n";
		done;
	[%expect{| Estimated number of nodes (left-heavy): |}]
