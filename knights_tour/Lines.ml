type t = string Seq.t

type 'a loader = string -> string Seq.t -> 'a 

let rec of_channel input () = 
  try (
    let line = input_line input in
    Seq.Cons(line, of_channel input)
  ) 
  with End_of_file -> Seq.Nil
  
let rec load_list terminator item_loader first_line input =
  if first_line=terminator then
    []
  else
    let first = item_loader first_line input in
    match Seq.uncons input with
    | None -> failwith "Unexpected end of input"
    | Some (first_line, input) ->
        first :: load_list terminator item_loader first_line input

let load_line first_line _input = first_line
  
  