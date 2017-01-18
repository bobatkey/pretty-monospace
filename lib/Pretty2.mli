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

