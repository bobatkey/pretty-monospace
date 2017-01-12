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
    ; break_dist : int
    ; break_type : [`Break|`NoBreak]
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
    ; break_dist = 0
    ; break_type = `NoBreak
    }

  let (^^) doc1 doc2 =
    match doc1, doc2 with
      | {node=Empty}, _ -> doc2
      | _, {node=Empty} -> doc1
      | doc1, doc2 ->
         { node       = Concat (doc1,doc2)
         ; flat_width =
             (match doc1.flat_width, doc2.flat_width with
               | None, _ | _, None -> None
               | Some w1, Some w2  -> Some (w1 + w2))
         ; break_dist =
             (match doc1.break_type with
               | `NoBreak -> doc1.break_dist + doc2.break_dist
               | `Break   -> doc1.break_dist)
         ; break_type =
             (match doc1.break_type with
               | `NoBreak -> doc2.break_type
               | `Break   -> `Break)
         }

  let text s =
    { node       = Text s
    ; flat_width = Some (String.length s)
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    ; break_dist = String.length s
    ; break_type = `NoBreak
    }

  let nest n doc =
    if n = 0 then doc
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".nest")
    else
      { node       = Nest (n, doc)
      ; flat_width = doc.flat_width
      ; break_dist = doc.break_dist
      ; break_type = doc.break_type
      }

  let hardbreak =
    { node       = HardBreak
    ; flat_width = None
    ; break_dist = 0
    ; break_type = `Break
    }

  let break_with s =
    { node       = Break s
    ; flat_width = Some (String.length s)
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    ; break_dist = 0
    ; break_type = `Break
    }

  let group doc =
    { node       = Group doc
    ; flat_width = doc.flat_width
    ; break_dist = doc.break_dist
    ; break_type = doc.break_type
    }

  let alignment_spaces n =
    if n = 0 then empty
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".alignment_spaces")
    else
      { node       = AlignSpaces n
      ; flat_width = Some 0
      ; break_dist = n
      ; break_type = `NoBreak
      }

  let align doc =
    { node       = Align doc
    ; flat_width = doc.flat_width
    ; break_dist = doc.break_dist
    ; break_type = doc.break_type
    }

  (****************************************************************************)
  let break =
    { node       = Break " "
    ; flat_width = Some 1
    ; break_dist = 0
    ; break_type = `Break
    }

  let spaces n =
    if n = 0 then empty
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".spaces")
    else
      { node       = Spaces n
      ; flat_width = Some n
      ; break_dist = n
      ; break_type = `NoBreak
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

(*
let rec fits left = function
  | _ when left < 0 ->
     false
  | (_,{node=Empty})::z ->
     fits left z
  | (i,{node=Concat (x,y)})::z ->
     fits left ((i,x)::(i,y)::z)
  | (_,{node=Text s})::z ->
     fits (left - String.length s) z
  | (_,{node=Spaces n | AlignSpaces n})::z ->
     fits (left - n) z
  | (i,{node=Align x | Nest (_,x) | Group x})::z ->
     fits left ((i,x)::z)
  | (_,{node=Break _ | HardBreak})::_ | [] ->
     true
*)

let flat output_text output_spaces doc =
  let stk = Stack.create () in
  Stack.push doc stk;
  while not (Stack.is_empty stk) do
    match Stack.pop stk with
      | {node=Empty | AlignSpaces _} ->
         ()
      | {node=Concat (x,y)} ->
         Stack.push y stk;
         Stack.push x stk
      | {node=Text s} ->
         output_text s
      | {node=Spaces n} ->
         output_spaces n
      | {node=Align x | Nest (_,x) | Group x} ->
         Stack.push x stk
      | {node=Break s} ->
         output_text s
      | {node=HardBreak} ->
         assert false
  done

let format_new output_text output_newline output_spaces width doc =
  let stk = Stack.create () in
  let push i d =
    match Stack.top stk with
      | exception Stack.Empty ->
         Stack.push (i, d, 0) stk
      | (_, {break_type=`Break;break_dist}, _) ->
         Stack.push (i, d, break_dist) stk
      | (_, {break_type=`NoBreak;break_dist}, k) ->
         Stack.push (i, d, break_dist+k) stk
  in
  push 0 doc;
  let column = ref 0 in
  while not (Stack.is_empty stk) do
    match Stack.pop stk with
      | (i,{node=Empty},_) ->
         ()

      | (i,{node=Concat (x,y)},_) ->
         push i y;
         push i x

      | (i,{node=Text s},_) ->
         output_text s;
         column := !column + String.length s

      | (i,{node=Spaces n},_)
      | (i,{node=AlignSpaces n},_) ->
         output_spaces n;
         column := !column + n

      | (i,{node=Align x},_) ->
         push !column x

      | (i,{node=Nest (j,x)},_) ->
         push (i+j) x

      | (i,{node=Break _},_)
      | (i,{node=HardBreak},_) ->
         output_newline ();
         output_spaces i;
         column := i

      | (i,{node=Group x; flat_width=Some flat_width},break_dist)
        when (width - !column - flat_width) >= break_dist ->
         flat output_text output_spaces x;
         column := !column + flat_width

      | (i,{node=Group x},_) ->
         push i x
  done


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
