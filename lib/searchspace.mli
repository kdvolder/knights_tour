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

(** Represents a search space constructed 'on demand' by calling function.
  This is useful to create compact/finite representations of infinite or very large searchspaces 
  (only the parts of a searchspace that are actually traversed will be constructed) *)
val defer : (unit -> 'a t)  -> 'a t

(** A searchpage containing a 'range' of values generate using a kind of 'while loop'.
    Expects three parameters. The first is a start value. The second a predicate
    that is the condition of the loop. The third parameter is a 'step' function. 
    
    For example to create a range of numbers 1 to 10 we do:
    {[range 1 ((>=) 10) ((+) 1)]}*)
val range : 'a -> ('a -> bool) -> ('a -> 'a) -> 'a t

(** Searchspace containing a range of integers ranging from lowerbound (1st parameter),
    to upperbound (2nd parameter). The bounds are inclusive. Example:
    {[int_range 1 10]}
    Produces numbers from 1 to 10 including 10.
    *)
val int_range : int -> int -> int t 

(** Search for the next solution. If a solution is found, returns it
    alongside a reduced searchspace that can be used to search for more solutions.
    Otherwise it returns None *)
val search : 'a t -> ('a * 'a t) option

(** Converts a searchspace into a [Seq] of its solutions *)
val to_seq : 'a t -> 'a Seq.t

(** searchspace containing all natural numbers. WARNING: must handle with care because
    it is an infinite searchspace.*)
val nats : int t

(** searchspace of all pairs of natural numbers *)
val nat_pairs : (int * int) t