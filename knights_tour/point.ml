type t = {x: int; y:int}

let compare {x=x1;y=y1} {x=x2;y=y2} =
  let xcomp = y1 - y2 in
  if xcomp<>0 then 
    xcomp
  else
    x1 - x2

module Set =
    Set.Make(struct type nonrec t = t let compare = compare end)
