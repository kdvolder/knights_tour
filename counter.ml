open Base

type t = int Map.M(String).t
(* type t = (string*int) list *)

let empty = Map.empty (module String)

let getCount counts line = 
  Map.find counts line |> Option.value ~default:0

let touch counts line = 
  Map.set counts ~key:line ~data:(getCount counts line) 

let toList = Map.to_alist ?key_order:None
