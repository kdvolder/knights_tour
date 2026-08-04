let rec with_separator str sep = function
  | [] -> ""
  | x::y::ys -> str x ^ sep ^ with_separator str sep (y::ys)
  | [x] -> str x

let rec lines_of_channel input () = 
  try (
    let line = input_line input in
    Seq.Cons(line, (lines_of_channel input))
  ) 
  with End_of_file -> Seq.Nil