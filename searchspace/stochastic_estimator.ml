(* Implementation of stochastic estimation functions *)

(* Add your implementation code here *)
(* Moved from searchspace.ml *)

open Searchspace
open Collections.Util

type decision = {
    chosen: int;
    choices: int
}

type rng = int -> int

let ( let* ) = bind

let rec random_walk rng space = inspect space |> function
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
let hello_world () = print_endline "hello"

let%expect_test "hello_world prints hello" =
	hello_world ();
	[%expect{|hello|}]

