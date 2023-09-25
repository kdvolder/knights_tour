(** A Dlist, short for double-ended list, is a purely functional 
  ordered list of elements that can be accessed both from the 
  front and the back. *)
module type S = Dlist_itf.S

include S
