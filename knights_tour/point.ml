type t = {x: int; y:int}

let compare {x=x1;y=y1} {x=x2;y=y2} =
  let xcomp = x1 - x2 in
  if xcomp<>0 then 
    xcomp
  else
    y1 - y2

