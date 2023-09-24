module Treequence = Treequence

type 'a t = 
  | Result of 'a
  | Fork of 'a t Treequence.t
  | Lazy of (unit -> 'a t)

let return x = Result x

let alt2 x y = Fork Treequence.(
  push x (singleton y)
)

let alt choices = Fork (List.fold_right Treequence.push choices Treequence.empty)

let empty = Fork Treequence.empty

let rec bind a f = match a with
  | Result a -> Lazy (fun () -> (f a))
  | Fork choices -> Fork ( 
    choices |> Treequence.map (fun choice -> bind choice f)
  )
  | Lazy l -> Lazy (fun () ->  bind (l ()) f)

let map f m = bind m (fun a -> return (f a))

let filter pred m = bind m (fun q -> if pred q then return q else empty)

let (|=>) = bind
let (|->) m f = map f m
let (|?>) m p = filter p m
let (++) = alt2

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

type 'a search_fun = 'a t -> ('a * 'a t) option

let cached_for ~iterations getter =
  let counter = ref 0 in
  let cache = ref (getter ()) in
  fun () -> begin
    counter := !counter + 1;
    if !counter >= iterations then begin
      counter := 0;
      cache := getter ()
    end;
    !cache
  end

let memfree = cached_for ~iterations:10000 Memfree.mem_free_ratio 

(** [scale_to_inifinity x] goes from 0 to infinity as x goes from 0 to 1 *)
let scale_to_inifinity x =
  let divider = 1.0 -. x in
  if divider <= 0.0 then
    Float.infinity
  else
    (x /. divider)

let limit_on_low_memory ~max_memory_ratio () = 
  let free_ratio = memfree () in
  let used_ratio = 1.0 -. free_ratio in
  scale_to_inifinity (used_ratio /. max_memory_ratio)

let rec breadth_search_aux limit stackmon steps stack =
  let steps = ref (steps + 1) in
  let pop worklist =
    let lmt = Float.of_int (Treequence.size worklist) *. limit () in
    if Float.of_int !steps > lmt then (
      (* broaden search by choosing the oldest choice point to explore further *)
      stackmon "pop_end" !steps worklist;
      steps := 0;
      Treequence.pop_end worklist
    ) else ( 
      (* narrow search by choosing the newest choice point to explore further (which tends to
         follow a single 'track/path/subtree' of choices until it is 'exhausted' / reaches a conclusion) *)
        stackmon "pop" !steps worklist;
        Treequence.pop worklist
    ) in
  match pop stack with 
  | None -> None
  | Some (item, stack) -> (match item with
    | Result x -> Some (x, Fork stack)
    | Fork choices -> Treequence.append choices stack 
        |> breadth_search_aux limit stackmon !steps
    | Lazy producer -> Treequence.push (producer ()) stack 
        |> breadth_search_aux limit stackmon !steps
  )
let default_limit () = 1.0

let breadth_search ?(limit=default_limit) ?(stack_mon=fun _ _ _ -> ()) space =
  breadth_search_aux limit stack_mon 0 (Treequence.singleton space) 

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

let%expect_test "1 ++ 2"  = (return 1 ++ return 2)
  |> to_seq
  |> Seq.iter (Printf.printf "%d; ")
  ;[%expect{| 1; 2; |}]
  
let%expect_test "defer (1 ++ 2)"  = defer (fun () -> (return 1 ++ return 2))
  |> to_seq
  |> Seq.iter (Printf.printf "%d; ")
  ;[%expect{| 1; 2; |}]

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
[4; 10] 
|> List.map (fun limit () -> Float.of_int limit)
|> List.iter (fun limit ->
  (
    let* num1 = int_range 1 5 in
    let* num2 = int_range 1 5 in
    return (num1 , num2)
  )
  |> to_seq ~search:(breadth_search ~limit)
  |> Seq.iter (fun (x, y) -> (Printf.printf "(%d, %d) " x y))
  ; Printf.printf("\n")
)
; [%expect{|
  (1, 1) (1, 2) (1, 3) (1, 4) (1, 5) (2, 1) (2, 2) (2, 3) (2, 4) (2, 5) (3, 1) (3, 2) (3, 3) (3, 4) (3, 5) (4, 1) (4, 2) (4, 3) (4, 4) (4, 5) (5, 1) (5, 2) (5, 3) (5, 4) (5, 5)
  (1, 1) (1, 2) (1, 3) (1, 4) (1, 5) (2, 1) (2, 2) (2, 3) (2, 4) (2, 5) (3, 1) (3, 2) (3, 3) (3, 4) (3, 5) (4, 1) (4, 2) (4, 3) (4, 4) (4, 5) (5, 1) (5, 2) (5, 3) (5, 4) (5, 5) |}]