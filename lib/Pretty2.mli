module PrettyStream : sig
  type t

  val create      : int -> Buffer.t -> t
  val text        : t -> string -> unit
  val start_group : t -> unit
  val end_group   : t -> unit
  val break       : t -> string -> unit
  val alignment_spaces : t -> int -> unit
  val start_nest  : t -> int -> unit
  val end_nest    : t -> unit
  val start_align : t -> unit
  val end_align   : t -> unit
  val finish      : t -> unit
end

type t

val (^^) : t -> t -> t
val empty : t
val text : string -> t
val break_with : string -> t
val alignment_spaces : int -> t
val group : t -> t
val nest : int -> t -> t
val align : t -> t

val render : int -> t -> string
