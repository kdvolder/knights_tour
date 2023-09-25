(** A Clist is a 'counted' list. Essentially, its just a list with a counter
    that tracks its number of elements. This makes it O(1) to get its size. *)

type 'a t = int * 'a list
val empty : 'a t
val size : 'a t -> int
val is_empty : 'a t -> bool
val singleton : 'a -> 'a t
val split_n : int -> 'a t -> ('a t * 'a t)
val split : 'a t -> ('a t * 'a t)
val append : 'a t -> 'a t -> 'a t
val reverse : 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val map : ('a -> 'b) -> 'c * 'a list -> 'c * 'b list
val cons: 'a -> 'a t -> 'a t
val (++): 'a t -> 'a t -> 'a t
val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list
val with_separator : ('a -> string) -> string -> 'a t -> string