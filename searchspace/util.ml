let rec with_separator str sep = function
  | [] -> ""
  | x::y::ys -> str x ^ sep ^ with_separator str sep (y::ys)
  | [x] -> str x
