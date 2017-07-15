(* (C) Robert Atkey 2013-2017, see LICENSE for more information. *)

(** Monospace Pretty Printing *)

(**{1 Monospace Pretty Printing}

   This is a pretty printing library for generating monospaced text
   with line breaks inserted at appropriate points to fit a given
   screen width. It is intended for generating nicely formatted and
   readable source code output or dumps of internal program values.

   The library is structured as a set of combinators (in
   {!Doc}) that are used to construct prettyprinter documents
   (values of type {!document}). Prettyprinter documents can then be
   rendered to monospaced output devices by the rendering functions
   listed below.

   FIXME: update this. The combinator interface of this library is based on Philip
   Wadler's {i A Prettier Printer}. The implementation is based on
   Christian Lindig's {i Strictly Pretty}. Some ideas and clarity were
   extracted from Doaitse Swierstra and Olaf Chitil's {i Linear,
   Bounded, Functional Pretty-Printing}.

   {2 Example: S-Expressions}

   The following code implements a converter from s-expressions to
   prettyprinter documents.

   {[
     open Pretty.Doc

     type sexp = Atom of string | List of sexp list

     let rec pp_sexp sexp =
       match sexp with
         | Atom atom ->
            text atom
         | List elems ->
            group (nest 2 (text "(" ^^ map_join pp_sexp break elems ^^ text ")"))
   ]}

   A pretty-printer document generated from an s-expression (or from
   anything else) can be rendered to standard output by calling the
   {!print} function. For example, the OCaml top-level
   declarations:
   {[
     let test =
       List [ Atom "hello"
            ; List [Atom "world"; Atom "dsfiuy"]
            ; List [Atom "sdif"; Atom "dfiu"]
            ; List [ Atom "kdsfh"
                   ; Atom "kdsjfh"
                   ; List [ Atom "dskjfh"
                          ; Atom "sdkfh"
                          ; Atom "dskjfh"
                          ]
                   ; Atom "kdsjh"
                   ; List [Atom "kdsjhf"; Atom "oxiv"]
                   ; Atom "disf"
                   ]
            ];;

     Pretty.Doc.print_endline (pp_sexp test);;
   ]}
   will generate the output:
   {v
     (hello
       (world dsfiuy)
       (sdif dfiu)
       (kdsfh kdsjfh (dskjfh sdkfh dskjfh) kdsjh (kdsjhf oxiv) disf))
   v}

   Line breaks have been inserted between sub-elements of each
   parenthesised list in an effort to fit the output into 80 columns
   (the default width). The places where line breaks are inserted
   corresponds to places were the {!Doc.break} combinator is
   used in the construction of the pretty-printing document. In this
   example, the [pp_sexp] function has placed them between each
   element of each parenthesised list of s-expressions. Decisions on
   whether or not to use line breaks or spaces are made for
   sub-documents of a pretty-printing document delimited by the
   {!Doc.group} combinator.

   If we specify a narrower width, the output will be broken
   differently. The command:
   {[
     Pretty.Doc.print_endline ~width:40 (pp_sexp test);;
   ]}
   produces:
   {v
     (hello
       (world dsfiuy)
       (sdif dfiu)
       (kdsfh
         kdsjfh
         (dskjfh sdkfh dskjfh)
         kdsjh
         (kdsjhf oxiv)
         disf))
   v}
   where different decisions on where to place line breaks has been
   made, forced by the constraint that the width should be less than
   40 characters.

   In both cases, the output has been indented. This is the effect of
   the {!Doc.nest} combinator, which specifies how much
   additional indentation is to be inserted after a line break. The
   library also provides {!Doc.align} that sets the
   intentation for the next line break to be the current column. This
   is useful for aligning items vertically. *)

(** {2 Types} *)

type output
(** Type of output specifications. Use the functions in {!Output} to
   construct values of this type. *)

type document
(** Type of pretty-printer documents. Use the functions in {!Doc} to
    construct values of this type. *)

type prettifier
(** Type of streaming pretty-printers. Use the functions in
   {!Stream} to construct streaming pretty-printers and to use
   them. *)

(** {2 Pretty-Printer Documents} *)

(** Pretty-printer documents. *)
module Doc : sig

  (** The abstract type of documents. This type declaration is here to
      enable this module to be used as a functor argument. *)
  type t = document

  (** {2 Base Combinators}

      This section lists the base combinators used to build documents
      for pretty-printing.

      The three basic combinators are {!empty}, {!(^^)} and {!text}. 

      
  *)

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
      + [forall d s. group (d ^^ text s) =~= group d ^^ text s]
      + [forall d n. group (nest n d) =~= nest d (group d)]
      + [forall d. group (align d) =~= align (group d)]
      + [forall d s n. text s ^^ nest n d =~= nest n (text s ^^ d)]
      + [forall d s n. nest n d ^^ text s =~= nest n (d ^^ text s)]
      + [forall d n1 n2. alignment_spaces n1 ^^ nest n2 d =~= nest n2 (alignment_spaces n1 ^^ d)]
      + [alignment_spaces 0 =~= empty]
      + [forall n1 n2. alignment_spaces n1 ^^ alignment_spaces n2 =~= alignment_spaces (n1 ^^ n2)]
      + [forall d s. align (test s ^^ nest (String.length s) d) =~= text s ^^ align d]
      + [forall d n. nest n (align d) =~= align d]
      + [forall d. align (align d) =~= align d]
      + [group empty =~= empty]
      + [forall n. nest n empty =~= empty]
      + [align empty =~= empty]
      + [forall d. group (group d) =~= group d]

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
      0 x0 ^^ sep ^^ pp 1 x1 ^^ ... ^^ sep ^^ pp n xn]. 

      It is also equivalent to [join sep (Array.to_list (Array.mapi pp
      [| x0; x1; ... ; xn |]))].  *)
  val map_join_array : (int -> 'a -> t) -> t -> 'a array -> t

  (** [wrap sep []] is equivalent to [empty]. [wrap sep (x::xs)] is
      equivalent to [join sep (x :: List.map (fun x -> group (break ^^
      x)) xs)]. *)
  val wrap : t -> t list -> t

  (** [wrap_array sep arr] is equivalent to [wrap sep (Array.to_list
      arr)]. *)
  val wrap_array : t -> t array -> t

  (** {2 Inspection of pretty-printer documents}*)

  (** [fold ~empty ~concat ~text ~break ~alignspaces ~nest ~align
     ~group doc] visits the nodes representing [doc] and calls the
     appropriate function argument on each one. Note that the
     structure visited will not necessarily be identical to the one
     implied by use of the construction functions above. However, it
     will be render-equivalent. The [~group] argument recieves
     additional argument s with the flat width of the sub-element, and
     the computed distance from the start of this group to the next
     line break opporunity following the group in the document. This
     makes [fold] non-local (FIXME: elaborate on this). *)
  val fold :
    empty:'a ->
    concat:('a -> 'a -> 'a) ->
    text:(string -> 'a) ->
    break:(string -> 'a) ->
    alignspaces:(int -> 'a) ->
    nest:(int -> 'a -> 'a) ->
    align:('a -> 'a) ->
    group:(int -> 'a -> 'a) ->
    document ->
    'a

  (**{2 Rendering of Prettyprinter Documents}

     The printing functions take an optional [width] argument that
     controls the decisions on whether to format each group in the
     document with or without linebreaks. In short, if there is enough
     space left on the current line (i.e., from the current position
     up to [width]) to format a group without line breaks, then the
     group is formatted without line breaks. Otherwise the group is
     formatted with line breaks.

     The optional [width] argument defaults to [80] in all the
     functions listed below.

     Note that specifying the [width] argument does not guarantee that
     the output will never exceed the given width. Uses of the base
     {!text} combinator will never be broken, and uses of {!nest} can
     force indentations above the specified width. *)

  val format : output -> ?width:int -> t -> unit

  val print : ?width:int -> t -> unit
end

(** {2 Streaming pretty printing} *)

(** Streaming pretty printing. *)
module Stream : sig

  type t = prettifier
  (** Type of streaming pretty printers. Use {!create} to construct a
      fresh streaming pretty printer. *)

  val create : ?width:int -> output -> t
  (** [create ?width out] creates a new streaming pretty printer that
     performs output using [out]. The optional [width] argument
     specifies the target width used for making line break
     decisions. *)

  val set_width : prettifier -> int -> unit
  (** [set_width pp n] sets the target width of the streaming pretty
      printer [pp] to [n].

      FIXME: how is this incorporated into the current decisions? *)

  (**{2 Pretty printing functions} *)

  val text : t -> string -> unit
  (** [text pp s] signals to the prettyprinter [pp] to print the text
      [s]. The string [s] should not contain newline characters. If it
      does, then the automatic linebreaking will not operate
      correctly. *)

  val char : t -> char -> unit
  (** [char pp c] signals to the prettyprinter [pp] to print the char
      [c]. The char [c] should not be a newline character. If it is,
      then the automatic linebreaking will not operate correctly. *)

  val start_group : t -> unit
  (** [start_group pp] starts a new group for the purposes of making
     line breaking decisions. Every group should be terminated by a
     corresponding {!end_group}. Groups should be correctly nested
     with respect to {!start_align}/{!start_nest} and
     {!end_nest_or_align} pairs. *)

  val end_group : t -> unit
  (** [end_group pp] ends a break-decision group started by
      {!start_group}. *)

  val break : t -> string -> unit
  (** [break pp s] signals the prettyprinter [pp] to emit the string
     [s] if in flat mode, or a newline and indentation to the current
     indentation level if in breaking mode. Decisions about whether to
     be in flat or breaking mode are made at the group level. Groups
     are indicated by {!start_group} and {!end_group}. *)

  val alignment_spaces : t -> int -> unit
  (** [alignment_spaces pp i] signals the prettyprinter [pp] to emit
     [n] spaces if in breaking mode, or nothing if in flat
     mode. Spaces produced by this combinator are not counted for the
     purposes of measuring groups. *)

  val start_nest : t -> int -> unit
  (** [start_nest pp i] opens a nest block and adds [i] to the current
     indentation level. Every such block must be closed by an
     {!end_nest_or_align}, and must be correctly nested with respect
     to {!start_group}-{!end_group} blocks. *)

  val start_align : t -> unit
  (** [start_align pp] opens an block in which the indentation level
     is set to the current column. Every such block must be closed by
     an {!end_nest_or_align}, and must be correctly nested with
     respect to {!start_group}-{!end_group} blocks. *)

  val end_nest_or_align : t -> unit
  (** [end_nest_or_align pp] closes a nest block opened by
     {!start_nest} or {!start_align}. *)

  val flush : t -> unit
  (** [flush pp] flushes the current state of the prettyprinter
     [pp]. There must be no open groups or indentation blocks. *)

end

(** {2 Output Specifications} *)

(** Output specifications for pretty-printing jobs. *)
module Output : sig

  type t = output
  (** Type of output specifications for pretty-printing jobs. *)

  val stdout : t
  (** Output to [stdout]. *)

  val stderr : t
  (** Output to [stderr]. *)

  val to_channel : out_channel -> t
  (** Output to the specified output channel. *)

  val to_buffer : Buffer.t -> t
  (** Output to a buffer. *)

  val custom :
    text:(string -> unit) ->
    char:(char -> unit) ->
    newline:(unit -> unit) ->
    spaces:(int -> unit) ->
    t
    (** Custom output specification. *)

end

