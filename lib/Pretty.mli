(** A Pretty Printing Library *)

(**{1 Pretty Printing Library} *)

(** A pretty printing library, based on Wadler's {i A Prettier
    Printer}, and Christian Lindig's {i Strictly Pretty}. Some ideas
    and clarity extracted from Doaitse Swierstra and Olaf Chitil's {i
    Linear, Bounded, Functional Pretty-Printing}. *)

(**{2 Primitive Combinators}

*)

(** The abstract type of documents. *)
type document

(** Concatenation of two documents. Concatenation of document is
    constant time. *)
val (^^) : document -> document -> document

(** The empty document. *)
val empty : document

(** A document just containing some text. For the formatting of
    documents to work correctly, the string argument must not contain
    newline characters (['\n']). Use the primitive {!break} or
    {!breakWith} combinators instead. *)
val text : string -> document

(** Increment the indentation added after each line break if the
    sub-document is formatted using line breaks. *)
val nest : int -> document -> document

(** A newline if the enclosing group is formatted with line breaks,
    otherwise a single space [" "]. *)
val break : document

(** A newline if the enclosing group is formatted with line breaks,
    otherwise the supplied string. For document formatting to work
    correctly, the string argument must not contain newline characters
    (['\n']). *)
val breakWith : string -> document

(** [alignSpc n] renders as [n] spaces if the enclosing group is
    formatted {i with} line breaks, and acts the same as {!empty}
    otherwise. *)
val alignSpc : int -> document

(** Create a grouped sub-document for which the decision to format
    with line breaks or without is made as a whole. *)
val group : document -> document

(** Set the indentation level to be the current column. *)
val align : document -> document

(**{2 Derived pretty-printing combinators} *)

(** [x ^+^ y] is equivalent to [x ^^ text " " ^^ y]. *)
val (^+^) : document -> document -> document

(** [x ^/^ y] is equivalent to [x ^^ break ^^ y]. *)
val (^/^) : document -> document -> document

(** [concat sep [x0; x1; ...; xn]] is equivalent to [x0 ^^ sep ^^ x1
    ^^ ... ^^ sep ^^ xn]. *)
val concat : document -> document list -> document

(** [map_concat_array pp sep [| x0; x1; ...; xn |]] is equivalent to
    [pp 0 x0 ^^ sep ^^ pp 1 x1 ^^ ... ^^ sep ^^ pp n xn]. *)
val map_concat_array : (int -> 'a -> document) -> document -> 'a array -> document

val wordwrap : string -> document

(**{3 Pretty printing of primitive data types} *)

(** Pretty print an [int] value using {!string_of_int}. *)
val int : int -> document

(** Pretty print a [bool] value as either ["true"] or ["false"]. *)
val bool : bool -> document

(** Pretty print a [float] value using {!string_of_float}. *)
val float : float -> document

(** Pretty print a unit value as ["()"]. *)
val unit : document

(** Pretty print a string value as an escaped string (according to
    OCaml lexical conventions) wrapped in quotes. *)
val string : string -> document

(**{3 Pretty printing of data structures} *)

val record : (string * document) list -> document

val list : document list -> document

val array : (int -> 'a -> document) -> 'a array -> document

val set : document list -> document

val tuple : document list -> document

val application : document -> document list -> document

val constructor : string -> document list -> document

(**{2 Rendering} 

   Each of the rendering functions below takes an optional [width]
   argument that controls the decisions on whether to format each
   group in the document with or without linebreaks. In short, if
   there is enough space left on the current line (i.e., from the
   current position up to [width]) to format a group without line
   breaks, then this is done. Otherwise the group is formatted with
   line breaks.

   The [width] argument defaults to [80] in all cases.

   Note that specifying the [width] argument does not guarantee that
   the output will never exceed the given width. Uses of the primitive
   {!text} combinator will never be broken, and uses of {!nest} can
   force indentations above the specified width.
*)

(** Print a document to [stdout]. *)
val print : ?width:int -> document -> unit

(** Print a document to [stderr]. *)
val prerr : ?width:int -> document -> unit

(** Print a document to an {!out_channel}. *)
val output : ?width:int -> out_channel -> document -> unit

(** Render a document to a string. *)
val to_string : ?width:int -> document -> string

(** Format a document with custom output functions. *)
val custom_output : ?width:int ->
  output_text:(string -> unit) ->
  output_newline:(unit -> unit) ->
  output_spaces:(int -> unit) ->
  document ->
  unit
