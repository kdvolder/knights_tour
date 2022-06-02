(**
  Utilities for reading input one line at a time. The basic idea is that different types
  of input which can be read one line at a time can all be converted into a [string Seq.t].
  Then any further manipulations / operations on this input can be done in a uniform
  way by operating on those.
*)

type t = string Seq.t
val of_channel : in_channel -> string Seq.t
