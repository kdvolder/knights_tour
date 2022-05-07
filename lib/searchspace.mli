type 'a t

(** A searchspace that contains a single solution *)
val return : 'a -> 'a t

(** A searchspace that is constructed using the result of another search as input *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Apply a function to every solution of search space *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Only retains solutions that match a given condition *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Represents a decision between multiple alternatives *)
val alt : 'a t list -> 'a t

(** Represents a decision between two alternatives *)
val alt2 : 'a t -> 'a t -> 'a t

(** A searchspace with no solutions *)
val empty : 'a t

(** A searchspace that is accessed by calling some side-effecting operation. 
    Such operations must be paired with a 'undo' operation so that the backtracking
    engine can undo the side-effect when backing out of exploring that searchspace *)
val withUndo : (unit -> 'a t) -> undo:(unit -> unit) -> 'a t

(** Represents a decision between two alternatives *)
val (++) : 'a t -> 'a t -> 'a t

(** Represents a lazily constructed search space, useful to create finite representations
  of infinite searchspaces *)
val defer : 'a t lazy_t  -> 'a t

val range : 'a -> ('a -> bool) -> ('a -> 'a) -> 'a t

(** Search for the next solution. If a solution is found, returns it
    alongside a reduced searchspace that can be used to search for more solutions.contents
    Otherwise it returns None *)
val search : 'a t -> ('a * 'a t) option

val to_seq : 'a t -> 'a Seq.t

(** searchspace containing all natural numbers. WARNING: must handle with care because
    it is an infinite searchspace *)
val nats : int t

(** searchspace of all pairs of natural numbers *)
val nat_pairs : (int * int) t