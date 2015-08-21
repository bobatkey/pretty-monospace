(* Each group is either output 'flat' if it fits on one line, or
   'broken' if it doesn't. Once the decision has been made to go flat,
   then nested groups will be laid out flat. *)

(* TODO:
   - flatten the 'fitting' part into state?
   - test it against the purely functional version
   - implement a proportionally-spaced version (using cairo?) (or Gg/Vg?)
*)

module type OrderedAbelianGroup = sig
  type t

  val zero    : t

  val plus    : t -> t -> t

  val negate  : t -> t

  val compare : t -> t -> int
end

module type RenderEngine = sig
  type t

  module Width : OrderedAbelianGroup

  val width_of_string : t -> string -> Width.t
  val width_of_indent : t -> int -> Width.t
  val output_string   : t -> string -> unit
  val output_newline  : t -> Width.t -> unit
end

(* FIXME: assumes one char takes one character space. *)
module OutChannel_RenderEngine
  : (RenderEngine
     with type t = out_channel
      and type Width.t = int)
  =
struct
  module Width = struct
    type t = int
    let zero = 0
    let plus = (+)
    let negate x = -x
    let compare x y = x - y
  end

  type t = out_channel

  let width_of_string ch str =
    String.length str

  let width_of_indent ch n =
    n

  let output_string ch str =
    output_string ch str

  let output_newline ch indent =
    output_char ch '\n';
    output_string ch (String.make indent ' ')
end

(* FIXME: assumes one char takes one space *)
module Buffer_RenderEngine :
  (RenderEngine
   with type t = Buffer.t
   and  type Width.t = int)
  =
struct
  module Width = struct
    type t = int
    let zero = 0
    let plus = (+)
    let negate x = -x
    let compare x y =
      Pervasives.compare x y
  end

  type t = Buffer.t

  let width_of_string buf str =
    String.length str

  let width_of_indent buf n =
    n

  let output_string buf str =
    Buffer.add_string buf str

  let output_newline buf indent =
    Buffer.add_char buf '\n';
    Buffer.add_string buf (String.make indent ' ')
end

type event =
  [ `Text       of string
  | `Break      of string
  | `StartNest  of int
  | `StartAlign
  | `EndNestOrAlign
  | `StartGroup
  | `EndGroup
  ]

module type StreamingPrettyPrinter = sig
  type t

  type output
  type width

  val create : output -> width -> t
  val emit   : t -> event -> unit
  val finish : t -> unit
end

module type Document_Combinators = sig
  type t

  val text  : string -> t
  val empty : t
  val (^^)  : t -> t -> t
  val break : t
  val break_with : string -> t
  val group : t -> t
  val nest  : int -> t -> t
  val align : t -> t
end
(*
module Document = struct
  type t =
    | Empty
    | Concat    of t * t
    | Text      of string
    | BreakWith of string
    | Group     of t
    | Nest      of int * t
    | Align     of t

  let text str =
    Text str

  let empty =
    Empty

  let (^^) document1 document2 =
    Concat (document1, document2)

  let break =
    BreakWith " "

  let break_with str =
    BreakWith str

  let group document =
    Group document

  let nest n document =
    Nest (n, document)

  let align document =
    Align document
end
*)

(*
module LazyDocument = struct

  type t = document Lazy.t
  and document =
    | Empty
    | Concat    of t * t
    | Text      of string
    | BreakWith of string
    | Group     of t
    | Nest      of int * t
    | Align     of t

  type sexp = Atom of string | SExp of sexp list

  (* the laziness should be *outside* the pattern matching. *)
  let rec pp_sexp sexp = lazy (pp_sexp_ sexp)
  and pp_sexp_ = function
    | Atom s -> Text s
    | SExp l -> Group (List.fold_left 

end
*)
(*
module HTML
  (* FIXME: take an underlying output as a parameter, possibly an
     HTML/XML-based outputer (for OCamlMVC) *)
  : (StreamingPrettyPrinter
     with type output = out_channel
      and type width  = float)
=
struct
  type output = out_channel
  type width = float
  type t = output

  let create ch width =
    output_string ch "<div class=\"pretty\" data-pretty-width=\"";
    output_string ch (string_of_float width);
    output_string ch "\">";
    ch

  let emit ch = function
    | `Text str ->
       output_string ch str
    | `Break str ->
       output_string ch "<span class=\"prettyBreak\" data-break-text=\"";
       output_string ch str;
       output_string ch "\">";
       output_string ch str;
       output_string ch "</span>"
    | `StartNest n ->
       output_string ch "<span class=\"prettyNest\" data-nest-indent=\"";
       output_string ch (string_of_int n);
       output_string ch "\">"
    | `StartAlign ->
       output_string ch "<span class=\"prettyAlign\">"
    | `StartGroup ->
       output_string ch "<span class=\"prettyGroup\">"
    | `EndNestOrAlign | `EndGroup ->
       output_string ch "</span>"

  let finish ch =
    output_string ch "</div>"
end
*)

module MakeRenderer
    (R : RenderEngine)
  : (StreamingPrettyPrinter
     with type output = R.t
      and type width  = R.Width.t) =
struct
  type output = R.t
  type width = R.Width.t

  type fitting_state =
    {         event_queue : event Queue.t
    ; mutable flat_depth  : int
    ; mutable remaining   : width
    }

  type stack_element =
    | RestoreIndent of R.Width.t
    | BackToBreak
    | NoChange

  type t =
    {         stack    : stack_element Stack.t
    ;         width    : width
    ;         output   : output
    ; mutable breaking : bool
    ; mutable indent   : width
    ; mutable column   : width
    ; mutable fitting  : fitting_state option
    }

  let create output width =
    { stack    = Stack.create ()
    ; width
    ; output
    ; breaking = true
    ; indent   = R.Width.zero
    ; column   = R.Width.zero
    ; fitting  = None
    }

  let rec emit state : event -> unit = function
    | `Text s         -> text_event state s
    | `Break s        -> break_event state s
    | `StartNest i    -> startnest_event state i
    | `StartAlign     -> startalign_event state
    | `EndNestOrAlign -> endnest_event state
    | `StartGroup     -> startgroup_event state
    | `EndGroup       -> endgroup_event state

  and check_fit state = match state.fitting with
    | Some fitting when R.Width.(compare fitting.remaining zero < 0) ->
       Stack.push NoChange state.stack;
       state.fitting <- None;
       Queue.iter (emit state) fitting.event_queue
    | Some _ | None ->
       ()

  and does_fit state = match state.fitting with
    | None ->
       ignore ()
    | Some fitting ->
       Stack.push BackToBreak state.stack;
       state.fitting <- None;
       state.breaking <- false;
       Queue.iter (emit state) fitting.event_queue

  and text_event state s = match state.fitting with
    | None ->
       print_string s;
       state.column <- R.Width.plus state.column (R.width_of_string state.output s)
    | Some fitting ->
       let s_width = R.width_of_string state.output s in
       fitting.remaining <- R.Width.(plus fitting.remaining (negate s_width));
       Queue.push (`Text s) fitting.event_queue;
       check_fit state

  and break_event state s = match state.fitting with
    | None when state.breaking ->
       R.output_newline state.output state.indent;
       state.column <- state.indent
    | None ->
       R.output_string state.output s;
       let s_width = R.width_of_string state.output s in
       state.column <- R.Width.(plus state.column s_width)
    | Some fitting when fitting.flat_depth <= 0 ->
       Queue.push (`Break s) fitting.event_queue;
       does_fit state
    | Some fitting ->
       let s_width = R.width_of_string state.output s in
       fitting.remaining <- R.Width.(plus fitting.remaining (negate s_width));
       Queue.push (`Break s) fitting.event_queue;
       check_fit state

  and startgroup_event state = match state.fitting with
    | None when state.breaking ->
       state.fitting <- Some
           { event_queue = Queue.create ()
           ; flat_depth  = 1
           ; remaining   = R.Width.(plus state.width (negate state.column))
           }
    | None ->
       Stack.push NoChange state.stack
    | Some fitting ->
       if fitting.flat_depth <> 0 then
         fitting.flat_depth <- fitting.flat_depth + 1;
       Queue.push `StartGroup fitting.event_queue

  and endgroup_event state = match state.fitting with
    | None ->
        (match Stack.pop state.stack with
          | NoChange ->
             ()
          | BackToBreak ->
             state.breaking <- true
          | exception Stack.Empty ->
             invalid_arg "Unexpected endgroup"
          | RestoreIndent _ ->
             invalid_arg "Unexpected endgroup")
    | Some fitting ->
       fitting.flat_depth <- fitting.flat_depth - 1;
       Queue.push `EndGroup fitting.event_queue

  and startnest_event state indent = match state.fitting with
    | None ->
       Stack.push (RestoreIndent state.indent) state.stack;
       let i_width = R.width_of_indent state.output indent in
       state.indent <- R.Width.plus state.indent i_width
    | Some fitting ->
       Queue.push (`StartNest indent) fitting.event_queue

  and startalign_event state = match state.fitting with
    | None ->
       Stack.push (RestoreIndent state.indent) state.stack;
       state.indent <- state.column
    | Some fitting ->
       Queue.push `StartAlign fitting.event_queue

  and endnest_event state = match state.fitting with
    | None ->
       (match Stack.pop state.stack with
         | RestoreIndent indent ->
            state.indent <- indent
         | _ ->
            invalid_arg "unexpected endnest"
         | exception Stack.Empty ->
            invalid_arg "unexpected endnest")
    | Some fitting ->
       Queue.push `EndNestOrAlign fitting.event_queue

  and end_event state = match state.fitting with
    | None ->
       if not (Stack.is_empty state.stack) then
         invalid_arg "stack underflow"
    | Some fitting ->
       does_fit state;
       end_event state

  let finish state =
    end_event state
end

(***********************************************************************)
module Combinators_of_Backend (SP : StreamingPrettyPrinter) : sig
  include Document_Combinators

  val render : SP.output -> SP.width -> t -> unit
end
  =
struct
  type t = SP.t -> unit

  let text str =
    fun sp ->
      SP.emit sp (`Text str)

  let empty =
    fun sp ->
      ()

  let (^^) doc1 doc2 =
    fun sp ->
      doc1 sp;
      doc2 sp

  let break =
    fun sp ->
      SP.emit sp (`Break " ")

  let break_with str =
    fun sp ->
      SP.emit sp (`Break str)

  let group doc =
    fun sp ->
      SP.emit sp `StartGroup;
      doc sp;
      SP.emit sp `EndGroup

  let nest n doc =
    fun sp ->
      SP.emit sp (`StartNest n);
      doc sp;
      SP.emit sp `EndNestOrAlign

  let align doc =
    fun sp ->
      SP.emit sp `StartAlign;
      doc sp;
      SP.emit sp `EndNestOrAlign

  let render output width doc =
    let sp = SP.create output width in
    doc sp;
    SP.finish sp
end
