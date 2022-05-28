(** A searchspace with solutions of a given type. For examle a type [int Searchspace.t]
    is a searchspace who's solutions are integers. It can be thought of a as lazy-computed
    collection of integers. The members of the collection can be discovered/produced incrementally 
    by a search process.
    
    The solutions to a searchspace may be an infinite collection. For example you could define
    a searchspace who's solutions are all prime numbers. 
    
    It is unspecified whether the solutions of a searchspace are cached or produced again upon
    every attempt to find them. The current implementation does not cache results ever. 
    It is generally unsafe to do so in the presence of side-effects. Future/alternatie implementations
    might try to cache results in some situations when it is deemed safe to do so. *)
type 'a t

(** A searchspace that contains a single solution. *)
val return : 'a -> 'a t

(** Monadic bind. This constructs a new searchspace by feeding the
    results of an existing searchspace as input to a subsequent 
    searchprocess. 
    *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Operator syntax for [bind] *)
val (|=>) : 'a t -> ('a -> 'b t) -> 'b t

(** Apply a function to every solution of search space. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Operator syntax for [map] *)
val (|->) : 'a t -> ('a -> 'b) -> 'b t

(** Only retains solutions that match a given condition. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Operator syntax for [filter] *)
val (|?>) : 'a t -> ('a -> bool) -> 'a t

(** Represents a decision between multiple alternatives. *)
val alt : 'a t list -> 'a t

(** Represents a decision between two alternatives. *)
val alt2 : 'a t -> 'a t -> 'a t

(** Represents a decision between two alternatives. *)
val (++) : 'a t -> 'a t -> 'a t

(** A searchspace with no solutions. *)
val empty : 'a t

(** A searchspace that is accessed by calling some side-effecting operation. 
    Such operations must be paired with a 'undo' operation so that the backtracking
    engine can undo the side-effect when backing out of exploring that searchspace.
    
    Note that if the a search space directly returns objects that contain this state
    then this state will be 'destroyed / changed' by the search-engine as it
    traverses the space. (Essentially the returned solution is only valid until you
    request the next solution).
    
    There are different ways to avoid problems caused by this. The easiest is probably
    to avoid stateful operations altoghether. Alternatively you can map some kind of
    'clone' operation search using [map] before returning result to the end-user.
    *)
val withUndo : (unit -> 'a t) -> undo:(unit -> unit) -> 'a t

(** Represents a search space constructed 'on demand' by calling function.
  This is useful to create compact/finite representations of infinite or very large searchspaces 
  (only the parts of a searchspace that are actually traversed will be constructed) *)
val defer : (unit -> 'a t)  -> 'a t

(** A searchspace containing a 'range' of values generated using a kind of 'while loop'.
    [range start cond step] produces values starting at [start] and apply the [step] function
    to the previous value to produce a next value. It does so as long as the [cond] is true.
    If the [cond] is [false] initially, then this produces an empty range. 
    
    For example to create a range of numbers 1 to 10 we do:
    {[range 1 ((>=) 10) ((+) 1)]}*)
val range : 'a -> ('a -> bool) -> ('a -> 'a) -> 'a t

(** Searchspace containing a range of integers ranging from lowerbound (1st parameter),
    to upperbound (2nd parameter). The bounds are inclusive. Example:
    {[int_range 1 10]}
    Produces numbers from 1 to 10 including 10.
    *)
val int_range : int -> int -> int t 

(** This module provides different search functions that may explore the choices
    in a searchspace in a different order. Other functions that such as [to_seq]
    may take a [search_fun] as a parameter (which will determine the order in
    which results are produced) *)
type 'a search_fun = 'a t -> ('a * 'a t) option

(** Search for the next solution in a depth-first fashion until a solution is found or
    the searchspace is exhausted. If a solution is found, returns it alongside a reduced 
    searchspace that can be used to search for more solutions.
    Otherwise it returns [None] *)
val search : 'a search_fun

(** Search in a breadth-first fashion. Note that a breadth-first search
    has a tendency to explore more and more parts of a searchpace at the same time as it keeps on
    expanding and exploring branches in parallet. This can have a tendency to requre 
    exponentionally more and more memory. To avoid a virtualy unbounded blow up in memory usage,
    a limit is put on the maximum number of simulataneously active paths. When this number is 
    reached or exceeded then the search reverst to a depth-first search which has the effect
    of exploring some branches in depth and this reducing the number of active branches.*)
val breadth_search : int ->  'a search_fun

(** Converts a searchspace into a [Seq] of its solutions. The solutions are 
    produced incrementally as required. So it is fine to convert a searchspace 
    of infinite solutions to a [Seq]. An optional [search_fun] may be provided
    to alter the order in which solutions are being generated. *)
val to_seq : ?search:'a search_fun -> 'a t -> 'a Seq.t

(** Represents a decision between multiple potentially infinite alternatives as
    given by the elements of a [Seq]*)
val of_seq : 'a Seq.t -> 'a t

(** Represents a decision between mupliple values as given by a list*)
val of_list : 'a list -> 'a t

(** searchspace containing all natural numbers. WARNING: must handle with care because
    it is an infinite searchspace.*)
val nats : int t

(** searchspace of all pairs of natural numbers *)
val nat_pairs : (int * int) t

(** filters out duplicate solutions. Note that this operation is quite expensive because
    it creates a Set data structure to keep track of all previously encountered elements
    in order to detect any duplicates. *)
val no_dup : ('a -> 'a -> int) -> 'a t -> 'a t

(** Represents a search space constructed in a recursive manner (i.e. it refers to itself). 
    The typical way to use this would be something like:

    [let nats = recursive (fun self -> return 1 ++ (self |-> ((+ 1))))]
    
val recursive : ('a t -> 'a t) -> 'a t *)
