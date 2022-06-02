type t = string Seq.t

let rec of_channel input () = 
  try (
    let line = input_line input in
    Seq.Cons(line, of_channel input)
  ) 
  with End_of_file -> Seq.Nil
  