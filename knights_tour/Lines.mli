(**
  Utilities for reading input one line at a time. The basic idea is that different types
  of input which can be read one line at a time can all be converted into a [string Seq.t].
  Then any further manipulations / operations on this input can be done in a uniform
  way by operating on those.
*)

type t = string Seq.t

(** Read data from a channel one line at a time. I.e this is like using the [input_line] funtion
from [Stdlib] and the creating a Seq of the results, until the end of the input is reached.*)
val of_channel : in_channel -> string Seq.t

type 'a loader = string -> string Seq.t -> 'a

(** [load_list terminator item_loader] produces a line-based loader which
    reads in a list of items by using [item_loader] to load one item, until
    the input matches an expected [terminator] string.*)
val load_list : string -> 'a loader -> 'a list loader

(** A loader that loads a single line of input and returns that as its result*)
val load_line : string loader
