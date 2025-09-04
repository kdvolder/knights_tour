open Knights_tour

include module type of Set.Make(Point)

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

(** Gets all 'variants' of a given PointSet. A variant is similar shape 
    obtained by applying rotation and mirroring transformations; and then
    applying [normalize_translation].*)
val variants : t -> t list

(** [variants_with_transform shape] returns a list of [(transform, variant)] pairs representing 
    all variants of a given pointset. A variant is a shape obtained by applying rotation and mirroring 
    transformations, followed by [normalize_translation]. The [transform] paired with each variant is a 
    function [t -> t] that applies a symmetry operation to the input pointset and produces the corresponding 
    variant when applied to the input. 
*)
val variants_with_transform : t -> ((t -> t) * t) list

(** Gets a canonical representation of a pointset that can be used to represent
    all variants. *)
val normalize : t -> t

(** Move all points an equal distance in both x and y coordinates *)
val translate : Point.t -> t -> t
