(* (C) Robert Atkey 2013, see LICENSE for more information. *)

(** Monospace Pretty Printing *)

(**{1 Monospace Pretty Printing}

   A pretty printing library for generating monospaced text with
   newlines inserted at appropriate points to fit a given screen
   width.

   The interface of this library is based on Philip Wadler's {i A
   Prettier Printer}, and Christian Lindig's {i Strictly Pretty}. Some
   ideas and clarity were extracted from Doaitse Swierstra and Olaf
   Chitil's {i Linear, Bounded, Functional Pretty-Printing}.

   {2 Example: S-Expressions}

   The following code implements a simple pretty-printer for a simple
   representation of s-expressions.

   {[
     open Pretty.Document

     type sexp = Atom of string | List of sexp list

     let rec pp_sexp sexp =
       match sexp with
         | Atom atom ->
            text atom
         | List elems ->
            group (nest 2 (text "(" ^^ map_join pp_sexp break elems ^^ text ")"))
   ]}


   {2 Building Pretty-Printer Documents}
*)

(** Type of pretty-printer documents. *)
type document

(** Combinators for building pretty-printer documents. *)
module Combinators : sig

  (** 
     FIXME: Introduction to the combinators: empty and text. break and
     group. hardbreak. alignment_spaces. nest and align.

     FIXME: the derived combinators
  *)

  (** The abstract type of documents. *)
  type t = document

  (** {2 Base Combinators}

      This section lists the base combinators used to build documents
      for pretty-printing. *)

  (** Concatenation of two documents. Concatenation of documents is
      constant time. *)
  val ( ^^ ) : t -> t -> t

  (** The empty document. *)
  val empty : t

  (** A document just containing some text. For the formatting of
      documents to work correctly, the string argument must not contain
      newline characters (['\n']) or tab characters (['\t']). Use the
      primitive {!break} or {!break_with} combinators, or the {!nest}
      and {!align} combinators instead. *)
  val text : string -> t

  (** A newline if the enclosing group is formatted with line breaks,
      otherwise the supplied string. For document formatting to work
      correctly, the string argument must not contain newline characters
      (['\n']) or tab characters (['\t']). See also {!break}. *)
  val break_with : string -> t

  (** A break that always renders as a newline plus indentation. Any
      group that contains a [hardbreak] is forced to render with line
      breaks. *)
  val hardbreak : t

  (** [alignment_spaces n] renders as [n] spaces if the enclosing group
      is formatted {i with} line breaks, and acts the same as {!empty}
      otherwise. The int [n] must be greater than [0], otherwise an
      [Invalid_argument] exception is raised. *)
  val alignment_spaces : int -> t

  (** Create a grouped sub-document for which the decision to format
      with line breaks or without is made as a whole. *)
  val group : t -> t

  (** [nest n d] increments the indentation added after each line break
      if the sub-document [d] is formatted using line breaks. The [n]
      argument must be greater than or equal to [0], otherwise an
      [Invalid_argument] exception is raised. *)
  val nest : int -> t -> t

  (** Set the indentation level to be the current column. *)
  val align : t -> t

  
  (** {3 Equational laws}

      The base combinators in this module obey the following equational
      laws, in the sense that the left and right side are
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

      {2 Derived Combinators} 

      The following combinators for building pretty-printer documents
      are all defined in terms of the base combinators defined
      above. *)

  (** A newline if the enclosing group is formatted with line breaks,
      otherwise a single space [" "]. Equivalent to [break_with " "]. *)
  val break : t

  (** [spaces n] represents a document of [n] spaces. Equivalent to
      [text (String.make ' ' n)]. The int [n] must be greater than or
      equal to [0], otherwise an [Invalid_argument] exception is
      raised. *)
  val spaces : int -> t

  (** [space] represents a document with a single space. Equivalent to
      [spaces 1] or [text " "]. *)
  val space : t

  (** [x ^+^ y] is equivalent to [x ^^ text " " ^^ y]. *)
  val ( ^+^ ) : t -> t -> t

  (** [x ^/^ y] is equivalent to [x ^^ break ^^ y]. *)
  val ( ^/^ ) : t -> t -> t

  (** [x ^//^ y] is equivalent to [x ^^ hardbreak ^^ y]. *)
  val ( ^//^ ) : t -> t -> t

  (** [join sep [x0; x1; ...; xn]] is equivalent to [x0 ^^ sep ^^ x1
      ^^ ... ^^ sep ^^ xn]. *)
  val join : t -> t list -> t

  (** [map_join pp sep [x0; x1; ...; xn]] is equivalent to [pp x0 ^^ sep
      ^^ pp x1 ^^ ... ^^ sep ^^ pp xn]. *)
  val map_join : ('a -> t) -> t -> 'a list -> t

  (** [join_array sep [| x0; x1; ...; xn |]] is equivalent to [x0 ^^ sep
      ^^ x1 ^^ ... ^^ sep ^^ xn]. *)
  val join_array : t -> t array -> t

  (** [map_join_array pp sep [| x0; x1; ...; xn |]] is equivalent to [pp
      0 x0 ^^ sep ^^ pp 1 x1 ^^ ... ^^ sep ^^ pp n xn]. *)
  val map_join_array : (int -> 'a -> t) -> t -> 'a array -> t

  (** [wrap sep []] is equivalent to {!empty}. [wrap sep (x::xs)] is
      equivalent to [join sep (x :: List.map (fun x -> group (break ^^
      x)) xs)]. *)
  val wrap : t -> t list -> t

  (** [wrap_array sep arr] is equivalent to [wrap sep (Array.to_list
      arr)]. *)
  val wrap_array : t -> t array -> t

(**/**)
  
  (**{2 Debugging}*)

  (** Generate a string representation of the underlying
      representation of a pretty-printer document. This is of interest
      for debugging purposes only. *)
  val to_string : t -> string
end

(**{2 Rendering} 

   Each of the rendering functions below takes an optional [width]
   argument that controls the decisions on whether to format each
   group in the document with or without linebreaks. In short, if
   there is enough space left on the current line (i.e., from the
   current position up to [width]) to format a group without line
   breaks, then the group is formatted without line breaks. Otherwise
   the group is formatted with line breaks.

   The optional [width] argument defaults to [80] in all the functions
   listed below.

   Note that specifying the [width] argument does not guarantee that
   the output will never exceed the given width. Uses of the base
   {!Combinators.text} combinator will never be broken, and uses of
   {!Combinators.nest} can force indentations above the specified
   width. *)

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

(** Pretty-print a document to an {!Pervasives.out_channel}. *)
val output : ?width:int -> out_channel -> document -> unit

(** Pretty-print a document to an {!Pervasives.out_channel}, followed
    by a newline. *)
val output_endline : ?width:int -> out_channel -> document -> unit

(** Pretty-print a document to a string. *)
val render : ?width:int -> document -> string

(** Format a document with custom output functions. *)
val custom : ?width:int ->
  output_text:(string -> unit) ->
  output_newline:(unit -> unit) ->
  output_spaces:(int -> unit) ->
  document ->
  unit
