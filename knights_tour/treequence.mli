(**
 A Treequence is data structure that represents a finite, ordered sequence
 of elements. I.e it is much like a list. 
 *)
type 'a t

(** An empty Treequence, contains no elements. *)
val empty : 'a t

(** Creates a Treequence containing a single element. *)
val singleton : 'a -> 'a t

(** [size t] returns the number of elements in [t]. This operation is O(1)*)
val size : 'a t -> int

(** [push el t] Adds an element to the front of [t] *)
val push : 'a -> 'a t -> 'a t

(** [pop el t] Removes an element from the front of t.*)
val pop : 'a t -> ('a * 'a t) option

(** [push_end el t] Adds an element to the end of [t]*)
val push_end : 'a -> 'a t -> 'a t

(** [pop_end el t] Removes an element from the end of [t]*)
val pop_end : 'a t -> ('a * 'a t) option

(** [append s1 s2] Creates Treequence that contains all elements of [s1] 
    followed by all elements of [s2]*)
val append : 'a t -> 'a t -> 'a t

(** Converts Treequence into a string revealing its internal structure. Useful
    for debugging and testing. *)
val to_string : ('a -> string) -> 'a t -> string