(**
  Utilities for reading input one line at a time. The basic idea is that different types
  of input which can be read one line at a time can all be converted into a [string Seq.t].
  Then any further manipulations / operations on this input can be done in a uniform
  way by operating on those.
*)

type t = string Seq.t

(** Read data from a channel one line at a time. I.e this is like using the [input_line] funtion
from [Stdlib] and the creating a Seq of the results, until the end of the input is reached.*)
val of_channel : in_channel -> t

(** Split string into lines *)
val of_string : string -> t

(** A ['a loarder] converts input into a value of type 'a. It is given the first line of
input as its first argument, and any additional lines can be read from the second parameter
as needed. (This mechanic allows a one line lookahead for making decisions about how to
parse data.*)
type 'a loader = string -> t -> 'a

(** [load_list terminator item_loader] produces a line-based loader which
    reads in a list of items by using [item_loader] to load one item, until
    the input matches an expected [terminator] string.*)
val load_list : string -> 'a loader -> 'a list loader

(** A loader that loads a single line of input and returns that as its result*)
val load_line : string loader

(** Use a loader to load something *)
val load : 'a loader -> t -> 'a
