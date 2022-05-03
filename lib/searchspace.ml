(* exception Bug of string *)

type 'a t = 
  | Result of 'a
  | Fork of 'a t * 'a t 
  | Lazy of 'a t lazy_t
  | Empty

let return x = Result x

let rec bind a f = match a with
  | Result a -> Lazy (lazy (f a))
  | Fork (l,r) -> Lazy (lazy (Fork (bind l f, bind r f)))
  | Lazy l -> Lazy (lazy (bind (Lazy.force l) f))
  | Empty -> Empty

let map f m = bind m (fun a -> return (f a))

let filter pred = (Fun.flip bind) (fun x -> if pred x then return x else Empty)

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
  | Lazy l -> search (Lazy.force l)

let defer l = Lazy l

let empty = Empty

let rec range from whle step = defer (lazy (
  if whle from then
    return from ++ (range (step from) whle step)
  else 
    empty
))

let int_range lo hi = range lo ((>=) hi) ((+) 1)

let rec ints_from start = (return start) ++ defer (lazy (ints_from (1 + start)))
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