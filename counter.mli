
(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts *)
val empty : t

(** Bump frequency count of a given string *)
val touch : t -> string -> t

(** Converts frequency counts into an assoc list *)
val toList : t -> (string * int) list
