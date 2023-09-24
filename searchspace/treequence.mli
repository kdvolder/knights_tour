include Dlist_itf.S
module Persistable :
  functor (A : Persist.Persistable) -> Persist.Persistable with type t = A.t t
