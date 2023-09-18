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

(** Search in a mixed breadth-first and depth first fashion. This search pattern strives 
    to strike a compromise between doing a depth-first and a breadth-first search. 
    
    Depth-first searches have the drawback for a very large searchspace that they will 
    tend to get stuck exploring very narrow 'subtree' of the entire space. This is determined
    by choices made early on in the search process. These choices lead such large search spaces
    that the search engine will never be able to backtrack out of them to explore other parts
    of the space.
    
    A breadth-first search on the other hand will explore branches in parallel (it keeps 'active
    branches in a queue and works on exploring each branch in turn a little bit, before moving
    on to exploring another branch.

    This has the advantage of exploring the search space more broadly instead of getting stuck
    in a narrow subspace determined by early choices. However for very large spaces 
    this will lead to an explosive growth in memory requirement (the queue grows
    larger and larger in an exponential fashion as the search space branches out in an exponential
    number of nodes in terms of the depth).

    The [breadth_search] function strikes a compromise between these two extremes by using
    a [Treequence] data structure to keep track of active branches. The [Treequence] can be used
    as either a stack or a queue because it allows pushing and poping of elements on either
    front or back. A compromise between branching out and exploring in depths is achieved by
    dynamically switching between using the [Treequence] either as a stack 
    or a queue. As the [Treequence] grows in size the ratio of operations using it as a stack vs
    a queue is gradually increased. Thus as the number of actively explored branches increases the
    tendency to explore in depth also increases; and the tendency to branch out decreases.
      
    Optional Parameters:

    - [limit]: determines how aggressively the search space is breadth exploration will be
               limited as the work queue grows in size. If set to 0 then there is no limit
               and the work queue is always used as a queue. If set to [n], then the number
               of [queue_ops / stack_ops] tends towards [n / active_branches]. The default 
               [limit] value is [1].
               
    - [stack_mon]: a function that is called on every step of the search allowing a caller
               to monitor progress and/or collect statistical data. 
               The default is a function that does nothing.

    *)
val breadth_search : ?limit:(unit -> float) ->  ?stack_mon:(string -> int -> 'a t Treequence.t -> unit) -> 'a search_fun

val limit_on_low_memory : max_memory_ratio:float -> unit -> float

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

(**
 A [Treequence] is 'pure/functional' data structure that represents a finite, 
 ordered sequence of elements. I.e it is much like a list. Unlike a typical list 
 implementation it supports efficient pushing and popping on both the front and back. 
 Thus it can be used interchangeably as a Stack or a Queue without much of a performance
 penalty. *)
module Treequence = Treequence