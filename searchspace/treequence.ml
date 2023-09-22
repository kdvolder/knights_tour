type 'a t =
| Empty
| Single of 'a
| Append of {sz: int; lt: 'a t; rt: 'a t }

let size = function
| Empty -> 0
| Single _ -> 1
| Append {sz;_} -> sz

let empty = Empty

let is_empty t = size t = 0

let singleton x = Single x

let append xs ys = match xs, ys with
| Empty, ys -> ys
| xs, Empty -> xs
| _ -> Append{sz=size xs + size ys; lt=xs; rt=ys}

let push x xs = append (singleton x) xs

let push_end x xs = append xs (singleton x)

let rec pop = function
| Empty -> None
| Single x -> Some (x, Empty) 
| Append {lt=Empty;rt;_} -> pop rt
| Append {lt=Single x; rt; _} -> Some(x,rt)
| Append {lt=Append{lt=a;rt=b;_};rt=c; _} -> 
    pop (append a (append b c))

let rec pop_end = function
| Empty -> None
| Single x -> Some (x, Empty)
| Append {lt; rt=Empty; _} -> pop lt
| Append {lt; rt=Single x; _} -> Some(x, lt)
| Append {lt=a; rt=Append{lt=b; rt=c; _}; _} ->
    pop_end (append (append a b) c)

let rec map f = function
| Empty -> Empty 
| Single x -> Single (f x)
| Append {sz; lt; rt} -> Append {sz; lt=map f lt; rt=map f rt}   

let rec to_string str = function
| Empty -> "nil"
| Single x -> str x
| Append{lt;rt;_} -> "[" ^ to_string str lt ^ " " ^ to_string str rt ^ "]" 

let%expect_test "pushes and pops" =
  let stack = empty 
    |> push 1
    |> push 2
    |> push 3 
    |> push 4
    |> push 5 in
  Printf.printf "stack: %s\n" (to_string Int.to_string stack);
  let rec pop_all stack =
  pop stack |> (function 
  | Some (top, rest) -> 
      Printf.printf "Popped: %d Rest: %s\n" top (to_string Int.to_string rest);
      pop_all rest
  | None -> Printf.printf "===end==="
  )
  in pop_all stack
  ;[%expect{|
    stack: [5 [4 [3 [2 1]]]]
    Popped: 5 Rest: [4 [3 [2 1]]]
    Popped: 4 Rest: [3 [2 1]]
    Popped: 3 Rest: [2 1]
    Popped: 2 Rest: 1
    Popped: 1 Rest: nil
    ===end=== |}]

let%expect_test "use as a queue" =
  let stack = empty 
    |> push 1
    |> push 2
    |> push 3 
    |> push 4 
    |> push 5 in
  Printf.printf "stack: %s\n" (to_string Int.to_string stack);
  let rec pop_all stack =
  pop_end stack |> (function 
  | Some (top, rest) -> 
      Printf.printf "Popped: %d Rest: %s\n" top (to_string Int.to_string rest);
      pop_all rest
  | None -> Printf.printf "===end==="
  )
  in pop_all stack
  ;[%expect{|
    stack: [5 [4 [3 [2 1]]]]
    Popped: 1 Rest: [[[5 4] 3] 2]
    Popped: 2 Rest: [[5 4] 3]
    Popped: 3 Rest: [5 4]
    Popped: 4 Rest: 5
    Popped: 5 Rest: nil
    ===end=== |}]
  
module Persistable (A : Persist.Persistable) = struct 
  type tt = A.t t
  type t = tt
  let rec save out = function 
  | Empty -> 
      output_char out '0'
  | Single v -> 
      output_char out '1'; 
      A.save out v
  | Append {lt;rt;_} -> 
      output_char out '2';
      save out lt;
      save out rt
  let rec load inp =
    let kind = input_char inp in
    match kind with
    | '0' -> Empty
    | '1' -> Single (A.load inp)
    | '2' -> 
        let lt = load inp in
        let rt = load inp in
        append lt rt
    | c -> raise (Failure (Printf.sprintf "Unexpected intput: '%c'" c))
end

module IntTreequence = Persistable(struct
  type t = int
  let save = output_binary_int
  let load = input_binary_int
end)

let%expect_test "save and load" = 
  let stack : int t = empty |> push 1 |> push 2 |> push 3 |> push 4 |> push 5 in
  let file = Filename.temp_file "test-save" ".bin" in
  Printf.printf "org   : %s\n" (to_string Int.to_string stack);
  Out_channel.with_open_bin file (fun out -> 
    IntTreequence.save out stack
  );
  let stack = In_channel.with_open_bin file (fun inp ->
    IntTreequence.load inp
  ) in
  Printf.printf "loaded: %s\n" (to_string Int.to_string stack);
  ;[%expect{|
    org   : [5 [4 [3 [2 1]]]]
    loaded: [5 [4 [3 [2 1]]]] |}]

let rec of_list = function
    [] -> empty
  | x::xs -> of_list xs |> push x

let%expect_test "of_list" =
  let it = of_list [1; 2; 3; 4; 5] in
  Printf.printf "%s\n" (to_string Int.to_string it)
  ;[%expect{| [1 [2 [3 [4 5]]]] |}]

let pop_and_drop op stack =
  op stack |> Option.get |> fun (_,s) -> s

let%expect_test "alternating stack and queue" =
  let it = ref (of_list [1; 2; 3; 4; 5]) in
  for i = 10 to 15 do
    it := !it |> pop_and_drop pop;
    it := !it |> push i;
    Printf.printf "pop: %s\n" (to_string Int.to_string !it);

    it := !it |> pop_and_drop pop_end;
    it := !it |> push_end i;
    Printf.printf "pop: %s\n" (to_string Int.to_string !it)
  done
  ;[%expect{|
    pop: [10 [2 [3 [4 5]]]]
    pop: [[[[10 2] 3] 4] 10]
    pop: [11 [2 [3 [4 10]]]]
    pop: [[[[11 2] 3] 4] 11]
    pop: [12 [2 [3 [4 11]]]]
    pop: [[[[12 2] 3] 4] 12]
    pop: [13 [2 [3 [4 12]]]]
    pop: [[[[13 2] 3] 4] 13]
    pop: [14 [2 [3 [4 13]]]]
    pop: [[[[14 2] 3] 4] 14]
    pop: [15 [2 [3 [4 14]]]]
    pop: [[[[15 2] 3] 4] 15] |}]
