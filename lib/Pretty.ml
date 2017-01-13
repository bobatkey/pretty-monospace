(* (C) Robert Atkey 2013, see LICENSE for more information. *)

module Combinators = struct
  type document =
    { node       : document_node
    ; flat_width : int
    (** Pre-computed width of this document were it to be formatted
        without line breaks. *)
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

    | Break of string
    (** the string if the containing group is formatted flat, a
        newline otherwise. *)

    | AlignSpaces of int
    (** [n] spaces if the containing group is formatted with line
        breaks, the empty string otherwise. The int [n] is always
        non-zero and positive. *)

    | Nest of int * document
    (** format the sub-document with the indentation level incremented
        by the given amount. The [int] value is always non-zero and
        positive. *)

    | Align of document
    (** format the sub-document with the indentation level set to the
        current column *)

    | Group of document
    (** group a sub-document for deciding whether to break lines or
        not. *)

  type t = document

  let rec to_string {node} = match node with
    | Empty         -> "Empty"
    | Concat(d1,d2) -> "Concat(" ^ to_string d1 ^ "," ^ to_string d2 ^ ")"
    | Text(t)       -> "Text(\""^String.escaped t^"\")"
    | Nest(n,d)     -> "Nest("^string_of_int n^","^to_string d^")"
    | Break(s)      -> "Break(\""^String.escaped s^"\")"
    | AlignSpaces i -> "AlignSpaces("^string_of_int i^")"
    | Align d       -> "Align("^to_string d^")"
    | Group d       -> "Group("^to_string d^")"

  let empty =
    { node       = Empty
    ; flat_width = 0
    ; break_dist = 0
    ; break_type = `NoBreak
    }

  let (^^) doc1 doc2 =
    match doc1, doc2 with
      | {node=Empty}, _ -> doc2
      | _, {node=Empty} -> doc1
      | doc1, doc2 ->
         { node       = Concat (doc1,doc2)
         ; flat_width = doc1.flat_width + doc2.flat_width
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
    ; flat_width = String.length s
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    ; break_dist = String.length s
    ; break_type = `NoBreak
    }

  let nest n doc =
    if n = 0 then doc
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".nest")
    else match doc.node with
      | Align _ -> doc
      | Nest (m, doc) ->
         { node       = Nest (n+m, doc)
         ; flat_width = doc.flat_width
         ; break_dist = doc.break_dist
         ; break_type = doc.break_type
         }
      | _ ->
         { node       = Nest (n, doc)
         ; flat_width = doc.flat_width
         ; break_dist = doc.break_dist
         ; break_type = doc.break_type
         }

  let break_with s =
    { node       = Break s
    ; flat_width = String.length s
      (* FIXME: UTF8? use Uucp.Break.tty_width_hint to measure it. *)
    ; break_dist = 0
    ; break_type = `Break
    }

  let group doc =
    match doc.node with
      | Group _ -> doc
      | _ ->
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
      ; flat_width = 0
      ; break_dist = n
      ; break_type = `NoBreak
      }

  let align doc =
    match doc.node with
      | Align _ -> doc
      | _ ->
         { node       = Align doc
         ; flat_width = doc.flat_width
         ; break_dist = doc.break_dist
         ; break_type = doc.break_type
         }

  (****************************************************************************)
  let break =
    { node       = Break " "
    ; flat_width = 1
    ; break_dist = 0
    ; break_type = `Break
    }

  let spaces n =
    if n = 0 then empty
    else if n < 0 then
      invalid_arg (__MODULE__ ^ ".spaces")
    else
      text (String.make n ' ')

  let space =
    spaces 1

  let (^+^) x y = x ^^ text " " ^^ y

  let (^/^) x y = x ^^ break ^^ y

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

let format output_text output_newline output_spaces width doc =
  let (+/) {break_type;break_dist} bd = match break_type with
    | `Break   -> break_dist
    | `NoBreak -> break_dist + bd
  in
  let rec flat = function
    | {node=Empty | AlignSpaces _}          -> ()
    | {node=Concat (x,y)}                   -> flat x; flat y
    | {node=Text s | Break s}               -> output_text s
    | {node=Align x | Nest (_,x) | Group x} -> flat x
  in
  let rec process column bd i = function
    | {node=Empty} -> column
    | {node=Concat (x,y)} ->
       let column = process column (y +/ bd) i x in
       process column bd i y
    | {node=Text s} ->
       output_text s; column + String.length s
    | {node=AlignSpaces n} ->
       output_spaces n; column + n
    | {node=Align x} ->
       process column bd column x
    | {node=Nest (j, x)} ->
       process column bd (i+j) x
    | {node=Break _} ->
       output_newline (); output_spaces i; i
    | {node=Group x; flat_width} ->
       if width - column - flat_width >= bd then
         (flat x;
          column + flat_width)
       else
         process column bd i x
  in
  ignore (process 0 0 0 doc)

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

let render ?(width=80) doc =
  let b = Buffer.create 2048 in
  format
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
