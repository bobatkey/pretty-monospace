type ast =
  | Empty
  | Concat of ast * ast
  | Text of string
  | Break of string
  | Nest of int * ast
  | Alignment_spaces of int
  | Group of ast
  | Align of ast

val to_string : ast -> string

module type LAYOUT_SPEC_LANGUAGE = sig
  type t
  val empty : t
  val ( ^^ ) : t -> t -> t
  val text : string -> t
  val break_with : string -> t
  val alignment_spaces : int -> t
  val group : t -> t
  val nest : int -> t -> t
  val align : t -> t
end

module AST : LAYOUT_SPEC_LANGUAGE with type t = ast

val interp : (module LAYOUT_SPEC_LANGUAGE with type t = 'a) -> ast -> 'a

val document_generator : (module LAYOUT_SPEC_LANGUAGE with type t = 'a) -> int -> 'a CamlCheck.Generator.t

val ast : ast CamlCheck.Domain.t
