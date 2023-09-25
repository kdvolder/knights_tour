module type Persistable = sig
  type t
  val save : Out_channel.t -> t -> unit
  val load : In_channel.t -> t
end
