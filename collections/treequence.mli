(**
 A [Treequence] is an implementation `Dlist` which keeps its elements in a tree. 
 Every operation that adds elements to Treequence is O(1). Operations that retrieve
 or remove elements from it will restructure the tree to expose either the front
 or back of the list for fast access. This allows for efficient accesses on both ends
 but there is a cost to be paid when switching between accessing from the 
 front/back or the back.*)

include Dlist_itf.S
module Persistable :
  functor (A : Persist.Persistable) -> Persist.Persistable with type t = A.t t
