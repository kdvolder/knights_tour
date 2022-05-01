type 'a t

(** A searchspace that contains a single solution *)
val result : 'a -> 'a t

(** Represents a decision between multiple alternatives *)
val alt : 'a t list -> 'a t

(** Represents a decision between two alternatives *)
val alt2 : 'a t -> 'a t -> 'a t

(** A searchspace with no solutions *)
val empty : 'a t

(** Represents a decision between two alternatives *)
val (++) : 'a t -> 'a t -> 'a t

(** Represents a lazily constructed search space, useful to create finite representations
  of infinite searchspaces *)
val defer : 'a t lazy_t  -> 'a t

(** Search for the next solution. If a solution is found, returns it
    alongside a reduced searchspace that can be used to search for more solutions.contents
    Otherwise it returns None *)
val search : 'a t -> ('a * 'a t) option

