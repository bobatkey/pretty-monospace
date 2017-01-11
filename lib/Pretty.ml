(* (C) Robert Atkey 2013, see LICENSE for more information. *)

module Combinators = struct
  type document =
    { node       : document_node
    ; flat_width : int option
    (** Pre-computed width of this document were it to be formatted
        without line breaks. [None] if the document contains a hard
        line break.

        Invariants: if node is [Empty], [Text], [Spaces], or [Break]
        then flat_width is always [Some x] for some [x]. *)
    }

  and document_node =
    | Empty
    (** the empty document *)

    | Concat of document * document
    (** two documents, one after the other *)

    | Text of string
    (** some text *)

    | Spaces of int
    (** [n] spaces, where [n] is always non-zero and positive. *)

    | Nest of int * document
    (** format the sub-document with the indentation level incremented
        by the given amount. The [int] value is always non-zero and
        positive. *)

    | Break of string
    (** the string if the containing group is formatted flat, a
        newline otherwise. *)

    | HardBreak
    (** a mandatory line break. *)

    | AlignSpaces of int
    (** [n] spaces if the containing group is formatted with line
        breaks, the empty string otherwise. The int [n] is always
        non-zero and positive. *)

    | Align of document
    (** format the sub-document with the indentation level to the
        current column *)

    | Group of document
    (** group a sub-document for deciding whether to break lines or
        not. *)

  type t = document

  let rec to_string {node} = match node with
    | Empty         -> "Empty"
    | Concat(d1,d2) -> "Concat(" ^ to_string d1 ^ "," ^ to_string d2 ^ ")"
    | Text(t)       -> "Text(\""^String.escaped t^"\")"
    | Spaces i      -> "Spaces("^string_of_int i^")"
    | Nest(n,d)     -> "Nest("^string_of_int n^","^to_string d^")"
    | Break(s)      -> "Break(\""^String.escaped s^"\")"
    | HardBreak     -> "HardBreak"
    | AlignSpaces i -> "AlignSpaces("^string_of_int i^")"
    | Align d       -> "Align("^to_string d^")"
    | Group d       -> "Group("^to_string d^")"

  let empty =
    { node       = Empty
    ; flat_width = Some 0
    }

  let (^^) doc1 doc2 =
    match doc1, doc2 with
      | {node=Empty}, _ -> doc2
      | _, {node=Empty} -> doc1
      | doc1, doc2 ->
         { node       = Concat (doc1,doc2)
         ; flat_width =
             match doc1.flat_width, doc2.flat_width with
               | None, _ | _, None -> None
               | Some w1, Some w2  -> Some (w1 + w2)
         }

  let text s =
    { node       = Text s
    ; flat_width = Some (String.length s)
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    }

  let nest n doc =
    if n = 0 then doc
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".nest")
    else
      { node       = Nest (n, doc)
      ; flat_width = doc.flat_width
      }

  let hardbreak =
    { node       = HardBreak
    ; flat_width = None
    }

  let break_with s =
    { node       = Break s
    ; flat_width = Some (String.length s)
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    }

  let group doc =
    { node       = Group doc
    ; flat_width = doc.flat_width
    }

  let alignment_spaces n =
    if n = 0 then empty
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".alignment_spaces")
    else
      { node       = AlignSpaces n
      ; flat_width = Some 0
      }

  let align doc =
    { node       = Align doc
    ; flat_width = doc.flat_width
    }

  (****************************************************************************)
  let break =
    { node       = Break " "
    ; flat_width = Some 1
    }

  let spaces n =
    if n = 0 then empty
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".spaces")
    else
      { node       = Spaces n
      ; flat_width = Some n
      }

  let space =
    spaces 1

  let (^+^) x y = x ^^ text " " ^^ y

  let (^/^) x y = x ^^ break ^^ y

  let (^//^) x y = x ^^ hardbreak ^^ y

  let join sep list =
    let i = ref 0 in
    List.fold_left
      (fun doc x ->
         let doc' = if !i = 0 then x else doc ^^ sep ^^ x
         in incr i; doc')
      empty
      list

  let map_join pp sep list =
    let i = ref 0 in
    List.fold_left
      (fun doc x ->
         let doc' = if !i = 0 then pp x else doc ^^ sep ^^ pp x
         in incr i; doc')
      empty
      list

  let join_array sep array =
    let i = ref 0 in
    Array.fold_left
      (fun doc x ->
         let doc' = if !i = 0 then x else doc ^^ sep ^^ x
         in incr i; doc')
      empty
      array

  let map_join_array pp sep array =
    let i = ref 0 in
    Array.fold_left
      (fun doc x ->
         let doc' = if !i = 0 then pp !i x else doc ^^ sep ^^ pp !i x
         in incr i; doc')
      empty
      array

  let wrap sep ds =
    fst (List.fold_left
           (fun (d,first) x ->
              ((if first then x else d ^^ sep ^^ group (break ^^ x)), false))
           (empty,true)
           ds)

  let wrap_array sep ds =
    fst (Array.fold_left
           (fun (d,first) x ->
              ((if first then x else d ^^ sep ^^ group (break ^^ x)), false))
           (empty,true)
           ds)
end

open Combinators

type document = t

type item = int * [`F|`B] * document

(* The stack switches from `B to `F only once. Can this be exploited
   to save a bit of space in the items? *)

(* [fits left items] determines whether or not we can fit 'items' up
   until the next break opportunity into 'left' columns. Running out
   of items, or a hardbreak, count as break opportunities. *)
let rec fits left : item list -> bool = function
  | (_,`F,_)::_                    -> assert false
  | _                when left < 0 -> false
  | []                             -> true
  | (i,_, {node=Empty})::z         -> fits left z
  | (i,m, {node=Concat (x,y)})::z  -> fits left ((i,m,x)::(i,m,y)::z)
  | (i,_, {node=Text s;flat_width=Some w})::z -> fits (left-w) z
  | (_,_, {node=Text _;flat_width=None})::_   -> assert false
  | (i,_, {node=Spaces n})::z      -> fits (left-n) z
  | (i,m, {node=Align x})::z       -> fits left ((i,m,x)::z)
  | (i,m, {node=Nest (j,x)})::z    -> fits left ((i,m,x)::z)
(*| (i,`F,{node=Break s})::z       -> assert false*)
  | (i,`B,{node=Break _})::z       -> true
  | (i,_, {node=HardBreak})::z     -> true
(*| (i,`F,{node=AlignSpaces n})::z -> assert false*)
  | (i,`B,{node=AlignSpaces n})::z -> fits (left-n) z
  | (i,`B,{node=Group x})::z       -> fits left ((i,`B,x)::z)
(*  | (_,`F,_)::_                    -> assert false*)

let format output_text output_newline output_spaces width doc =
  let rec process column = function
    | [] ->
       ()

    | (i,m,{node=Empty})::z ->
       process column z

    | (i,m,{node=Concat (x,y)})::z ->
       process column ((i,m,x)::(i,m,y)::z)

    | (i,m,{node=Text s; flat_width=Some w})::z ->
       output_text s;
       process (column + w) z

    | (i,m,{node=Text s; flat_width=None})::z ->
       assert false

    | (i,m,{node=Spaces n})::z ->
       output_spaces n;
       process (column + n) z

    | (i,m,{node=Align x})::z ->
       process column ((column,m,x)::z)

    | (i,m,{node=Nest (j,x)})::z ->
       process column ((i+j,m,x)::z)

    | (i,`F,{node=Break s; flat_width=Some w})::z ->
       output_text s;
       process (column + w) z

    | (i,`F,{node=Break s; flat_width=None})::z ->
       assert false

    | (i,`B,{node=Break s})::z ->
       output_newline ();
       output_spaces i;
       process i z

    | (i,`F,{node=HardBreak})::z ->
       assert false

    | (i,`B,{node=HardBreak})::z ->
       output_newline ();
       output_spaces i;
       process i z

    | (i,`F,{node=AlignSpaces n})::z ->
       process column z

    | (i,`B,{node=AlignSpaces n})::z ->
       output_spaces n;
       process (column + n) z

    | (i,`F,{node=Group x})::z ->
       process column ((i,`F,x)::z)

    | (i,_,{node=Group x;flat_width=Some flat_width})::z
      when fits (width-column-flat_width) z ->
       assert (List.for_all (fun (_,m,_) -> m = `B) z);
       process column ((i,`F,x)::z)

    | (i,_,{node=Group x})::z ->
       process column ((i,`B,x)::z)
  in
  process 0 [(0,`B,doc)]

let rec fits left = function
  | _ when left < 0 -> false
  | [] -> true
  | (i,{node=Empty})::z         -> fits left z
  | (i,{node=Concat (x,y)})::z  -> fits left ((i,x)::(i,y)::z)
  | (i,{node=Text s})::z        -> fits (left - String.length s) z
  | (i,{node=Spaces n})::z
  | (i,{node=AlignSpaces n})::z -> fits (left - n) z
  | (i,{node=Align x})::z
  | (i,{node=Nest (_,x)})::z
  | (i,{node=Group x})::z       -> fits left ((i,x)::z)
  | (i,{node=Break _})::z
  | (i,{node=HardBreak})::z     -> true

let flat output_text output_spaces doc =
  let rec process = function
    | [] ->
       ()
    | {node=Empty}::z ->
       process z
    | {node=Concat (x,y)}::z ->
       process (x::y::z)
    | {node=Text s; flat_width=Some w}::z ->
       output_text s;
       process z
    | {node=Text _}::z ->
       assert false
    | {node=Spaces n}::z ->
       output_spaces n;
       process z
    | {node=Align x|Nest (_,x)|Group x}::z ->
       process (x::z)
    | {node=Break s; flat_width=Some w}::z ->
       output_text s; process z
    | {node=Break s; flat_width=None}::z ->
       assert false
    | {node=HardBreak}::_ ->
       assert false
    | {node=AlignSpaces n}::z ->
       process z
  in
  process [doc]

let format_new output_text output_newline output_spaces width doc =
  let rec process column = function
    | [] ->
       ()

    | (i,{node=Empty})::z ->
       process column z

    | (i,{node=Concat (x,y)})::z ->
       process column ((i,x)::(i,y)::z)

    | (i,{node=Text s})::z ->
       output_text s;
       process (column + String.length s) z

    | (i,{node=Spaces n})::z
    | (i,{node=AlignSpaces n})::z ->
       output_spaces n;
       process (column + n) z

    | (i,{node=Align x})::z ->
       process column ((column,x)::z)

    | (i,{node=Nest (j,x)})::z ->
       process column ((i+j,x)::z)

    | (i,{node=Break _})::z
    | (i,{node=HardBreak})::z ->
       output_newline ();
       output_spaces i;
       process i z

    | (i,{node=Group x; flat_width=Some flat_width})::z
      when fits (width-column-flat_width) z ->
       flat output_text output_spaces x;
       process (column + flat_width) z

    | (i,{node=Group x})::z ->
       process column ((i,x)::z)
  in
  process 0 [(0,doc)]


(******************************************************************************)
let output ?(width=80) ch doc =
  format
    (fun s  -> output_string ch s)
    (fun () -> output_char ch '\n')
    (fun n  -> output_string ch (String.make n ' '))
    width
    doc

let output_endline ?(width=80) ch doc =
  output ~width ch doc;
  output_char ch '\n'

let print ?(width=80) doc =
  output ~width stdout doc

let print_endline ?(width=80) doc =
  output_endline ~width stdout doc

let prerr ?(width=80) doc =
  output ~width stderr doc

let prerr_endline ?(width=80) doc =
  output_endline ~width stderr doc

let render ?(width=80) ~old doc =
  let b = Buffer.create 2048 in
  (if old then format else format_new)
    (fun s  -> Buffer.add_string b s)
    (fun () -> Buffer.add_char b '\n')
    (fun n  -> Buffer.add_string b (String.make n ' '))
    width
    doc;
  Buffer.contents b

let custom ?(width=80) ~output_text ~output_newline ~output_spaces doc =
  format
    output_text
    output_newline
    output_spaces
    width
    doc
