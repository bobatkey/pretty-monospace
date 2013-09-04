(* Each group is either output 'flat' if it fits on one line, or
   'broken' if it doesn't. Once the decision has been made to go flat,
   then nested groups will be laid out flat.
*)

(* TODO:
   - flatten the 'fitting' part into state?
   - test it against the purely functional version
   - implement a proportionally-spaced version (using cairo?) (functorised?)
*)

module type AbelianGroup = sig
  type t
  val zero   : t
  val plus   : t -> t -> t
  val negate : t -> t
end

module type RenderEngine = sig
  type t

  module Width : AbelianGroup

  val width_of_string : t -> string -> Width.t
  val width_of_indent : t -> int -> Width.t
  val output_string   : t -> string -> unit
  val output_newline  : t -> int -> unit
end

type event =
  [ `Text       of string
  | `Break      of string
  | `StartNest  of int
  | `StartAlign
  | `EndNest
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

(* val reset : t -> unit *)

(*
  val text : t -> string -> unit
  val break : t -> string -> unit
  val open_group : t -> unit
  val close_group : t -> unit
  val open_nest : t -> int -> unit
  val open_align : t -> unit
  val close_nest_or_align : t -> unit
  val finish : t -> unit
*)
end

(*
module Make
  (R : RenderEngine)
  : (StreamingPrettyPrinter
     with type output = R.t
     and  type width  = R.Width.t)
  =
struct
  type t = unit
  type output = 
end
*)

type fitting_state =
    { event_queue : event Queue.t
    ; mutable flat_depth : int
    ; mutable remaining  : int
    }

type stack_element =
  | RestoreIndent of int
  | BackToBreak
  | NoChange

type state =
    { stack : stack_element Stack.t
    ; width : int
    ; mutable breaking : bool
    ; mutable indent   : int
    ; mutable column   : int
    ; mutable fitting  : fitting_state option
    }

let init_state width =
  { stack    = Stack.create ()
  ; width    = width
  ; breaking = true
  ; indent   = 0
  ; column   = 0
  ; fitting  = None
  }

let rec process_event state : event -> unit = function
  | `Text s      -> text_event state s
  | `Break s     -> break_event state s
  | `StartNest i -> startnest_event state i
  | `StartAlign  -> startalign_event state
  | `EndNest     -> endnest_event state
  | `StartGroup  -> startgroup_event state
  | `EndGroup    -> endgroup_event state

and check_fit state = match state.fitting with
  | Some fitting when fitting.remaining < 0 ->
    Stack.push NoChange state.stack;
    state.fitting <- None;
    Queue.iter (process_event state) fitting.event_queue
  | _ ->
    ()

and does_fit state = match state.fitting with
  | None -> ignore ()
  | Some fitting ->
    Stack.push BackToBreak state.stack;
    state.fitting <- None;
    state.breaking <- false;
    Queue.iter (process_event state) fitting.event_queue

and text_event state s = match state.fitting with
  | None ->
    print_string s;
    state.column <- state.column + String.length s
  | Some fitting ->
    fitting.remaining <- fitting.remaining - String.length s;
    Queue.push (`Text s) fitting.event_queue;
    check_fit state

and break_event state s = match state.fitting with
  | None when state.breaking ->
    print_newline ();
    print_string (String.make state.indent ' ');
    state.column <- state.indent
  | None ->
    print_string s;
    state.column <- state.column + String.length s
  | Some fitting when fitting.flat_depth <= 0 ->
    Queue.push (`Break s) fitting.event_queue;
    does_fit state
  | Some fitting ->
    fitting.remaining <- fitting.remaining - String.length s;
    Queue.push (`Break s) fitting.event_queue;
    check_fit state

and startgroup_event state = match state.fitting with
  | None when state.breaking ->
    state.fitting <- Some
      { event_queue = Queue.create ()
      ; flat_depth  = 1
      ; remaining   = state.width - state.column
      }
  | None ->
    Stack.push NoChange state.stack
  | Some fitting ->
    if fitting.flat_depth <> 0 then
      fitting.flat_depth <- fitting.flat_depth + 1;
    Queue.push `StartGroup fitting.event_queue

and endgroup_event state = match state.fitting with
  | None -> begin
    try
      match Stack.pop state.stack with
        | NoChange ->
          ()
        | BackToBreak ->
          state.breaking <- true
        | RestoreIndent _ ->
          raise (Invalid_argument "Unexpected endgroup")
    with Stack.Empty ->
      raise (Invalid_argument "Unexpected endgroup")
  end
  | Some fitting ->
    fitting.flat_depth <- fitting.flat_depth - 1;
    Queue.push `EndGroup fitting.event_queue

and startnest_event state indent = match state.fitting with
  | None ->
    Stack.push (RestoreIndent state.indent) state.stack;
    state.indent <- state.indent + indent
  | Some fitting ->
    Queue.push (`StartNest indent) fitting.event_queue

and startalign_event state = match state.fitting with
  | None ->
    Stack.push (RestoreIndent state.indent) state.stack;
    state.indent <- state.column
  | Some fitting ->
    Queue.push `StartAlign fitting.event_queue

and endnest_event state = match state.fitting with
  | None -> begin
    try
      match Stack.pop state.stack with
        | RestoreIndent indent ->
          state.indent <- indent
        | _ ->
          raise (Invalid_argument "unexpected endnest")
    with Stack.Empty ->
      raise (Invalid_argument "unexpected endnest")
  end
  | Some fitting ->
    Queue.push `EndNest fitting.event_queue

and end_event state = match state.fitting with
  | None ->
    if not (Stack.is_empty state.stack) then
      failwith "stack underflow"
  | Some fitting ->
    does_fit state;
    end_event state

(***********************************************************************)
(* Functional interface *)
type doc =
  | DText of string
  | DEmpty
  | DAppend of doc * doc
  | DBreak of string
  | DGroup of doc
  | DNest of int * doc
  | DAlign of doc

let text str = DText str
let empty = DEmpty
let (^^) x y = DAppend (x,y)
let break = DBreak " "
let group d = DGroup d
let nest i d = DNest (i,d)
let align d = DAlign d

let pp width doc =
  let s = init_state width in
  let rec process = function
    | DText str ->
      text_event s str
    | DEmpty ->
      ()
    | DAppend (d1, d2) ->
      process d1;
      process d2
    | DBreak str ->
      break_event s str
    | DGroup d ->
      startgroup_event s;
      process d;
      endgroup_event s
    | DNest (i,d) ->
      startnest_event s i;
      process d;
      endnest_event s
    | DAlign d ->
      startalign_event s;
      process d;
      endnest_event s
  in
  process doc;
  end_event s
