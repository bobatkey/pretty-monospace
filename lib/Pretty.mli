(** A Pretty Printing Library *)

(**{1 Pretty Printing Library} *)

(** A pretty printing library, based on Philip Wadler's {i A Prettier
    Printer}, and Christian Lindig's {i Strictly Pretty}. Some ideas
    and clarity extracted from Doaitse Swierstra and Olaf Chitil's {i
    Linear, Bounded, Functional Pretty-Printing}. *)

(**{2 Primitive Combinators}

   This section lists the primitive combinators used to build
   documents for pretty-printing.
*)

(** The abstract type of documents. *)
type document

(** The primitive combinators in this section obey the following
    equational laws, in the sense that the left and right side are
    interchangable within any {!document} expression. The ranges of
    quantification are as follows:

    - [d], [d1], [d2], [d3] stand for any value of type {!document}.
    - [n], [n1], [n2] stand for any natural number - i.e., any [int]
    [i] such that [i >=0].
    - [s], [s1], [s2] stand for any string of printable characters,
    including spaces [' '].
    - [c] stands for any {!document} expression with a single hole.
    The notation [c[d]] stands for the plugging of [d] for the
    hole in [c].

    The laws:

    + [forall d. d =~= empty ^^ d]
    + [forall d. d =~= d ^^ empty]
    + [forall d1 d2 d3. d1 ^^ (d2 ^^ d3) =~= (d1 ^^ d2) ^^ d3]
    + [forall s1 s2. text s1 ^^ text s2 =~= text (s1 ^ s2)]
    + [text "" =~= empty]
    + [forall d s. group (text s ^^ d) =~= text s ^^ group d]
    + [forall d n. group (nest n d) =~= nest d (group d)]
    + [forall d. group (align d) =~= align (group d)]
    + [forall d s n. text s ^^ nest n d =~= nest n (test s ^^ d)]
    + [forall d n1 n2. alignment_spaces n1 ^^ nest n2 d =~= nest n2 (alignment_spaces n1 ^^ d)]
    + [alignment_spaces 0 =~= empty]
    + [forall d s. align (test s ^^ nest (String.length s) d) =~= text s ^^ align d]
    + [forall d n. nest n (align d) =~= align d]
    + [forall d. align (align d) =~= align d]
    + [group empty =~= empty]
    + [forall n. nest n empty =~= empty]
    + [align empty =~= empty]
    + [forall d. group (group d) =~= group d]
    + [forall c. group (c[hardbreak]) =~= c[hardbreak]]
*)

(** Concatenation of two documents. Concatenation of document is
    constant time. *)
val ( ^^ ) : document -> document -> document

(** The empty document. *)
val empty : document

(** A document just containing some text. For the formatting of
    documents to work correctly, the string argument must not contain
    newline characters (['\n']) or tab characters (['\t']). Use the
    primitive {!break} or {!break_with} combinators, or the {!nest} and
    {!align} combinators instead. *)
val text : string -> document

(** [nest n d] increments the indentation added after each line break
    if the sub-document [d] is formatted using line breaks. The [n]
    argument must be greater than or equal to [0], otherwise an
    [Invalid_argument] exception is raised. *)
val nest : int -> document -> document

(** A newline if the enclosing group is formatted with line breaks,
    otherwise the supplied string. For document formatting to work
    correctly, the string argument must not contain newline characters
    (['\n']) or tab characters (['\t']). *)
val break_with : string -> document

(** A break that always renders as a newline plus indentation. Any
    group that contains a [hardbreak] is forced to render with line
    breaks. *)
val hardbreak : document

(** [alignment_spaces n] renders as [n] spaces if the enclosing group
    is formatted {i with} line breaks, and acts the same as {!empty}
    otherwise. The int [n] must be greater than [0], otherwise an
    [Invalid_argument] exception is raised. *)
val alignment_spaces : int -> document

(** Create a grouped sub-document for which the decision to format
    with line breaks or without is made as a whole. *)
val group : document -> document

(** Set the indentation level to be the current column. *)
val align : document -> document


(**{2 Derived pretty-printing combinators} *)

(** A newline if the enclosing group is formatted with line breaks,
    otherwise a single space [" "]. Equivalent to [break_with " "]. *)
val break : document

(** [spaces n] represents a document of [n] spaces. Equivalent to
    [text (String.make ' ' n)]. The int [n] must be greater than or
    equal to [0], otherwise an [Invalid_argument] exception is
    raised. *)
val spaces : int -> document

(** [space] represents a document with a single space. Equivalent to
    [spaces 1] or [text " "]. *)
val space : document

(** [x ^+^ y] is equivalent to [x ^^ text " " ^^ y]. *)
val ( ^+^ ) : document -> document -> document

(** [x ^/^ y] is equivalent to [x ^^ break ^^ y]. *)
val ( ^/^ ) : document -> document -> document

(** [x ^//^ y] is equivalent to [x ^^ hardbreak ^^ y]. *)
val ( ^//^ ) : document -> document -> document

(** [concat sep [x0; x1; ...; xn]] is equivalent to [x0 ^^ sep ^^ x1
    ^^ ... ^^ sep ^^ xn]. *)
val concat : document -> document list -> document

(** [map_concat_array pp sep [| x0; x1; ...; xn |]] is equivalent to
    [pp 0 x0 ^^ sep ^^ pp 1 x1 ^^ ... ^^ sep ^^ pp n xn]. *)
val map_concat_array : (int -> 'a -> document) -> document -> 'a array -> document

(** [wrap sep []] is equivalent to {!empty}. [wrap sep (x::xs)] is
    equivalent to [concat sep (x :: List.map (fun x -> group (break ^^ x))
    xs)]. *)
val wrap : document -> document list -> document

(** [indent n d] indents by [n] spaces and then sets the indentation
    column to be the current column. [indent n d] is equivalent to
    [text (String.make n ' ') ^^ align d]. The int [n] must be greater
    than or equal to [0], otherwise an [Invalid_argument] exception is
    raised. *)
val indent : int -> document -> document

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
    OCaml lexical conventions) wrapped in quotes. [string s] is
    equivalent to [text ("\"" ^ String.escaped s ^ "\"")]. *)
val string : string -> document

(** Pretty print a character wrapped in single quotes, according to
    the OCaml lexical conventions. [char c] is equivalent to [text
    ("\'" ^ Char.escaped c ^ "\'")]. *)
val char : char -> document

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
   breaks, then the group is formatte without line breaks. Otherwise
   the group is formatted with line breaks.

   The optional [width] argument defaults to [80] in all the functions
   listed below.

   Note that specifying the [width] argument does not guarantee that
   the output will never exceed the given width. Uses of the primitive
   {!text} combinator will never be broken, and uses of {!nest} can
   force indentations above the specified width.
*)

(** Pretty-print a document to [stdout]. *)
val print : ?width:int -> document -> unit

(** Pretty-print a document to [stdout], followed by a newline
    character. *)
val print_endline : ?width:int -> document -> unit

(** Pretty-print a document to [stderr]. *)
val prerr : ?width:int -> document -> unit

(** Pretty-print a document to [stderr], followed by a newline
    character. *)
val prerr_endline : ?width:int -> document -> unit

(** Pretty-print a document to an {!out_channel}. *)
val output : ?width:int -> out_channel -> document -> unit

(** Pretty-print a document to an {!out_channel}, followed by a
    newline. *)
val output_endline : ?width:int -> out_channel -> document -> unit

(** Pretty-print a document to a string. *)
val to_string : ?width:int -> document -> string

(** Format a document with custom output functions. *)
val custom_format : ?width:int ->
  output_text:(string -> unit) ->
  output_newline:(unit -> unit) ->
  output_spaces:(int -> unit) ->
  document ->
  unit
