type 'a t = 
  | Result of 'a
  | Fork of 'a t * 'a t 
  | Lazy of (unit -> 'a t)
  | WithUndo of (unit -> 'a t) * (unit -> unit)
  | Empty

let return x = Result x

let rec bind a f = match a with
  | Result a -> Lazy (fun () -> (f a))
  | Fork (l,r) -> Fork (bind l f, bind r f)
  | Lazy l -> Lazy (fun () ->  bind (l ()) f)
  | Empty -> Empty
  | WithUndo (action, undo) -> 
       let action = fun () -> bind (action ()) f in
       WithUndo (action, undo)
let (|=>) = bind

let map f m = bind m (fun a -> return (f a))
let (|->) m f = map f m
let filter pred m = bind m (fun x -> if pred x then return x else Empty)
let (|?>) m p = filter p m

let withUndo action ~undo = WithUndo (action, undo)

let alt2 x y = Fork (x, y)

let rec alt = function
  | [] -> Empty
  | (x::xs) -> Fork (x, alt xs)

let (++) = alt2

let rec search = function 
  | Empty -> None
  | Result r -> Some (r, Empty)
  | Fork (left, right) -> ( 
      match search left with
      | Some (left, leftRest) -> Some (left, Fork (leftRest, right)) 
      | None -> search right
  )
  | Lazy l -> l () |> search
  | WithUndo (action, undo) ->
      let space = action () in
      match search space with 
      | None -> undo (); None
      | Some (first, rest) ->
          Some (first, withUndo (fun () -> rest) ~undo:undo)

let defer l = Lazy l

let empty = Empty

let rec range from whle step = defer (fun () -> (
  if whle from then
    return from ++ (range (step from) whle step)
  else 
    empty
))

let int_range lo hi = range lo ((>=) hi) ((+) 1)

let rec ints_from start = (return start) ++ defer (fun () -> (ints_from (1 + start)))
let nats = ints_from 0

let rec find_all space = match search space with
 | Some (first, rest) -> first :: find_all rest
 | None -> []

let to_dispenser (searchspace : 'a t) = 
    let state = ref (Some searchspace) in
    fun () -> 
      match !state with
      | None -> None
      | Some searchspace -> (
          match search searchspace with
          | None -> None
          | Some (first, rest) -> (
            state := Some rest;
            Some first
          ) 
      )

let to_seq (searchspace:'a t) = Seq.of_dispenser (to_dispenser searchspace)

let%expect_test "range 1..4" = 
  let searchspace = int_range 1 4 in
  find_all searchspace |> List.iter (fun result ->
    Format.printf "%d; " result
  ) ; [%expect{| 1; 2; 3; 4; |}]

let (let*) = bind 

let%expect_test "sum of two ranges" =
    (
      let numbers = int_range 1 4 in
      let* x = numbers in
      let* y = numbers in
        return (Format.sprintf "%d + %d = %d" x y (x + y))
    ) 
    |> find_all
    |> List.iter print_endline
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

let nat_pairs =
  let* x = nats in
  let* y = int_range 0 x in
  return (x,y)

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

