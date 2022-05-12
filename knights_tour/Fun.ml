include Stdlib.Fun

let rec repeat n f x =
  if n > 0 then
    f x |> repeat (n - 1) f
  else
    x

