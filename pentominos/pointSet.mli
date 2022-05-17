open Knights_tour

include module type of Set.Make(Point)

(** Checks whether a set of points is 'coherent'. What this means is
    that every points in the set can be reached from every other point in
    the set via its neighbours. If we consider a pointset to represent
    a polyomino, then it means the pointset represents a single puzzle
    piece (rather two or more disconnected 'islands' of points). 
    *)
val is_coherent : t -> bool

(** Find the smallest [x] coordinate in a pointset.*)
val min_x : t -> int

(** Find the largest [x] coordinate in a pointset.*)
val max_x : t -> int

(** Find the smallest [y] coordinate in a pointset.*)
val min_y : t -> int

(** Find the largest [y] coordinate in a pointset.*)
val max_y : t -> int

(** Takes a 'string image' of a pointset and parses it. A string image is just 
    a multiline string where each character indicates whether or not the square/point
    at the corresponding location is part of the set. 
    
    Leading and trailing whitespace on each line are ignored. The remaining characters
    are interpreted as follows:

    - [.] means the corresponding square is [b not] in the set
    - any other character means that it is.
    *)
val of_string : string -> t

val to_string : t -> string

(** Gets the set of adjacent points. A point is adjacent if it satisfies both:
    - it is a 'neighbour of any one of the point in the input; and
    - it is not a point in the input itself *)
val adjacent: t -> t

(** Translates the pointset so that all points [x] and [y] coordinates are greater or equal to 0; 
and have the smallest possible values given these conditions (i.e there is at 
least one point with [x = 0], and one point (possibly a different one) with [y = 0])) *)
val normalize_translation : t -> t