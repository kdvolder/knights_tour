type 'a t = 
  | Result of 'a
  | Fork of 'a t Treequence.t
  | Lazy of (unit -> 'a t)
  | WithUndo of (unit -> 'a t) * (unit -> unit)

let return x = Result x

let alt2 x y = Fork Treequence.(
  append (singleton x) (singleton y)
)

let alt choices = Fork (List.fold_right Treequence.push choices Treequence.empty)

let empty = Fork Treequence.empty

let rec bind a f = match a with
  | Result a -> Lazy (fun () -> (f a))
  | Fork choices -> Fork ( 
    choices |> Treequence.map (fun choice -> bind choice f)
  )
  | Lazy l -> Lazy (fun () ->  bind (l ()) f)
  | WithUndo (action, undo) -> 
       let action = fun () -> bind (action ()) f in
       WithUndo (action, undo)

let map f m = bind m (fun a -> return (f a))

let filter pred m = bind m (fun q -> if pred q then return q else empty)

let (|=>) = bind
let (|->) m f = map f m
let (|?>) m p = filter p m
let (++) = alt2

let withUndo action ~undo = WithUndo (action, undo)

let defer l = Lazy l

let rec range from whle step = defer (fun () -> (
  if whle from then
    return from ++ range (step from) whle step
  else 
    empty
))

let int_range lo hi = range lo ((>=) hi) ((+) 1)

let rec search = function 
  | Result r -> Some (r, empty)
  | Fork choices -> (match Treequence.pop choices with 
    | None -> None
    | Some (first, rest) -> (match search first with
      | None -> search (Fork rest)
      | Some (found, first_rest) -> Some (found, alt2 first_rest (Fork rest))
    )
  )   
  | Lazy l -> l () |> search
  | WithUndo (action, undo) ->
      let space = action () in
      match search space with 
      | None -> undo (); None
      | Some (first, rest) ->
          Some (first, withUndo (fun () -> rest) ~undo:undo)

type 'a search_fun = 'a t -> ('a * 'a t) option

let rec breadth_search_aux limit stack =
  let pop worklist =
    if Treequence.size worklist < limit then
      (* broaden search by choosing the oldest choice point to explore further *)
      Treequence.pop_end worklist
    else 
      (* narrow search by choosing the newest choice point to explore further (which tends to
         follow a single 'track/path/subtree' of choices until it is 'exhausted' / reaches a conclusion) *)
      Treequence.pop worklist
    in
  match pop stack with 
  | None -> None
  | Some (item, stack) -> (match item with
    | Result x -> Some (x, Fork stack)
    | Fork choices -> Treequence.append choices stack 
        |> breadth_search_aux limit
    | Lazy producer -> Treequence.push (producer ()) stack 
        |> breadth_search_aux limit
    | WithUndo (action, undo) ->
      match action () |> search with 
      | None -> undo (); None
      | Some (first, rest) ->
          Some (first, withUndo (fun () -> rest) ~undo:undo)
  )
let breadth_search limit space =
  breadth_search_aux limit (Treequence.singleton space) 

let rec to_seq ?(search=search) space () =
  match search space with
  | None -> Seq.Nil
  | Some (fst,rst) -> Seq.Cons (fst, to_seq ~search rst) 

let rec of_list = function
  | [] -> empty
  | x::xs -> return x ++ of_list xs

let rec ints_from start = return start ++ defer (fun () -> (ints_from (1 + start)))
let nats = ints_from 0

let rec of_seq alts = Lazy (fun () ->
  match Seq.uncons alts with
  | None -> empty
  | Some(first, rest) -> return first ++ of_seq rest
)

let ( let* ) = bind

let nat_pairs =
  let* x = nats in
  let* y = int_range 0 x in
  return (x,y)

let set_of_compare (type a) (compare : a -> a -> int) =
  let module Comp : Set.OrderedType with type t = a = struct
    type t = a
    let compare = compare
  end in
  let module SetOf = Set.Make(Comp) in
  (module SetOf : Set.S with type elt = a)
  
let no_dup (type a) (compare : a -> a -> int) inputs =
  let module InputSet = (val set_of_compare compare : Set.S with type elt = a) in
  inputs |> to_seq
  |> InputSet.of_seq
  |> InputSet.to_seq
  |> of_seq
  
let%expect_test "range 1..4" = 
  let searchspace = int_range 1 4 in
  to_seq searchspace |> Seq.iter (fun result ->
    Format.printf "%d; " result
  ) ; [%expect{| 1; 2; 3; 4; |}]

let%expect_test "sum of two ranges" =
    (
      let numbers = int_range 1 4 in
      let* x:int = numbers in
      let* y:int = numbers in
        return (Format.sprintf "%d + %d = %d" x y (x + y))
    ) 
    |> to_seq
    |> Seq.iter print_endline
    ; [%expect{|
      1 + 1 = 2
      1 + 2 = 3
      1 + 3 = 4
      1 + 4 = 5
      2 + 1 = 3
      2 + 2 = 4
      2 + 3 = 5
      2 + 4 = 6
      3 + 1 = 4
      3 + 2 = 5
      3 + 3 = 6
      3 + 4 = 7
      4 + 1 = 5
      4 + 2 = 6
      4 + 3 = 7
      4 + 4 = 8 |}]

let%expect_test "find some results in infinite searchspace" = nats
    |> to_seq |> Seq.take 5 
    |> Seq.iter (Format.printf "%d; ")
    ; [%expect{| 0; 1; 2; 3; 4; |}]

let%expect_test "infinite tuple walk" = nat_pairs
  |> to_seq |> Seq.take 10 
  |> Seq.iter (fun (x,y) -> Format.printf "(%d,%d); " x y)
  ; [%expect{| (0,0); (1,0); (1,1); (2,0); (2,1); (2,2); (3,0); (3,1); (3,2); (3,3); |}]

let%expect_test "stateful search" = 
  let state = ref 1 in
  let multiply mul = (
      (fun trace -> state:=!state*mul; trace ^ " x" ^ Int.to_string mul),
      (fun () -> state:=!state/mul)
  ) in
  let rec search trace  = 
    let number = !state in
    if number >= 10 then
      return (trace ^ " = " ^ Int.to_string number)
    else 
      let moves = [ multiply 2; multiply 3 ] in
      moves 
      |> List.map (fun (doer, undo) ->
        withUndo (fun () -> search (doer trace))
        ~undo: undo 
      )
      |> alt
  in search (Int.to_string !state)
    |> to_seq
    |> Seq.iter print_endline
  ;[%expect{|
    1 x2 x2 x2 x2 = 16
    1 x2 x2 x2 x3 = 24
    1 x2 x2 x3 = 12
    1 x2 x3 x2 = 12
    1 x2 x3 x3 = 18
    1 x3 x2 x2 = 12
    1 x3 x2 x3 = 18
    1 x3 x3 x2 = 18
    1 x3 x3 x3 = 27 |}]

let%expect_test "1 ++ 2"  = (return 1 ++ return 2)
  |> to_seq
  |> Seq.iter (Printf.printf "%d; ")
  ;[%expect{| 1; 2; |}]
  
let%expect_test "defer (1 ++ 2)"  = defer (fun () -> (return 1 ++ return 2))
  |> to_seq
  |> Seq.iter (Printf.printf "%d; ")
  ;[%expect{| 1; 2; |}]

let recursive generator = 
  let rec self = Lazy (fun () -> generator self) 
  in self

let%expect_test "recursive" = 
  let numbers = recursive (fun numbers ->  
    return 1 ++
    (numbers |-> ((+) 1)) 
  ) in 
  numbers |> to_seq 
  |> Seq.take 5
  |> Seq.iter (fun x ->
    Printf.printf "%d; " x
  ) 
  ;[%expect{| 1; 2; 3; 4; 5; |}]

let%expect_test "no_dup" =
  (
    let* num1 = int_range 1 5 in
    let* num2 = int_range 1 5 in
    return (num1 * num2)
  )
  |> no_dup Int.compare
  |> to_seq
  |> Seq.iter (Printf.printf "%d; ")
  ; [%expect{| 1; 2; 3; 4; 5; 6; 8; 9; 10; 12; 15; 16; 20; 25; |}]

let%expect_test "breadth_search" =
[4; 10] |> List.iter (fun limit ->
  (
    let* num1 = int_range 1 5 in
    let* num2 = int_range 1 5 in
    return (num1 , num2)
  )
  |> to_seq ~search:(breadth_search limit)
  |> Seq.iter (fun (x, y) -> (Printf.printf "(%d, %d) " x y))
  ; Printf.printf("\n")
)
; [%expect]