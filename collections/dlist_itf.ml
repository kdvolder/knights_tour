module type S = sig

  (** A ['a t] is a Dlist containing elements of type ['a]. A Dlist, short for double-ended
      list, is a purely functional ordered list of elements that can be accessed both from
      the front and the back. *)
  type 'a t 

  (** A name for the datatype, there are different implementation of
      this interface, you can check this name to identify which implementation
      you are using. *)
  val name : string
  
  (** An empty Dlist, contains no elements. *)
  val empty : 'a t
  
  (** [is_empty t] is true iff there are no elements in [t]. *)
  val is_empty : 'a t -> bool
  
  (** [map f t] applies a function to each element of t, creating a new Dlist with the results. *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  
  (** Creates a Dlist containing a single element. *)
  val singleton : 'a -> 'a t
  
  (** [size t] returns the number of elements in [t]. This operation is O(1). *)
  val size : 'a t -> int
  
  (** [push el t] Adds an element to the front of [t]. *)
  val push : 'a -> 'a t -> 'a t
  
  (** [pop el t] Removes an element from the front of t.*)
  val pop : 'a t -> ('a * 'a t) option
  
  (** [push_end el t] Adds an element to the end of [t]. *)
  val push_end : 'a -> 'a t -> 'a t
  
  (** [pop_end el t] Removes an element from the end of [t]. *)
  val pop_end : 'a t -> ('a * 'a t) option
  
  (** [append s1 s2] Creates Dlist that contains all elements of [s1] 
      followed by all elements of [s2]. *)
  val append : 'a t -> 'a t -> 'a t
  
  (** [of_list elements] Creates a [Dlist] from the elements of the list. The front
         of the list will become the front ot the [Dlist]. The first element you [pop]
         from the [Dlist] will be the first element in the list.*)
  val of_list : 'a list -> 'a t
  
  (** Converts Dlist into a string revealing its internal structure. Useful
      for debugging and testing. *)
  val to_string : ('a -> string) -> 'a t -> string
  
end