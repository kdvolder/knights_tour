exception Bug of string

type 'a t = 
  | Result of 'a
  | Fork of 'a t * 'a t 
  | Lazy of 'a t lazy_t
  | Empty

let ( >== ) = Option.bind

(* reduce1 simplifies a search space by one step, bringing it one step closer
to being in weak normal form.  It returns 'None' to signal that
a given searchspace is already in 'weak' normal form. *)
let rec reduce1 = function
   | Result _ -> Option.None
   | Fork (Empty, r) -> Option.Some r
   | Fork (l, r) -> reduce1 l >== (fun l -> Option.Some(Fork (l, r))) 
   | Lazy l -> Option.Some (Lazy.force l)
   | Empty -> Option.None

(* reduce a searchspace into 'Weak Normal Form' (WNF). Intuitively a searchSpace is in 
  WNF if we can look at the top node and its direct children and easily determine whether:
     - it has a solution or not 
     - get the first solution (if there is one) *)
let rec reduce search0 =
    match reduce1 search0 with
    | Some reduced -> reduce reduced
    | None -> search0

let result x = Result x

let alt2 x y = Fork (x, y)

let rec alt = function
  | [] -> Empty
  | (x::xs) -> Fork (x, alt xs)

let (++) = alt2

let search (space:'a t) = match reduce space with
  | Result (x:'a) -> Option.Some (x, Empty)
  | Fork (Result x, rest) -> Option.Some (x, rest)
  | Empty -> Option.None
  | _ -> raise (Bug "Searchspace not in WNF: probably a bug in reduce")

let defer l = Lazy l

let empty = Empty