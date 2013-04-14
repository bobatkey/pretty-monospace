(* (C) Robert Atkey 2013, see LICENSE for more information. *)

let lift2 f x y = match x,y with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (f x y)

type document =
    { node       : document_node
    ; flat_width : int option
    (** Pre-computed width of this document were it to be formatted
        without line breaks. [None] if the document contains a hard
        line break. *)
    }

and document_node =
  | Empty (** the empty document *)
  | Append   of document * document (** two documents, one after the other *)
  | Text     of string (** some text *)
  | Spaces   of int (** some spaces *)
  | Nest     of int * document (** increment the indentation level *)
  | Break    of string (** the string if the containing group is formatted flat, a newline otherwise *)
  | HardBreak (** a mandatory line break *)
  | AlignSpaces of int (** 'n' spaces if the containing group is formatted with line breaks, nothing otherwise *)
  | Align    of document (** set the indentation level to the current column *)
  | Group    of document (** group a sub-document for deciding whether to break lines or not *)

let rec string_of_document {node} = match node with
  | Empty         -> "Empty"
  | Append(d1,d2) -> "Append(" ^ string_of_document d1 ^ "," ^ string_of_document d2 ^ ")"
  | Text(t)       -> "Text(\""^String.escaped t^"\")"
  | Spaces i      -> "Spaces("^string_of_int i^")"
  | Nest(n,d)     -> "Nest("^string_of_int n^","^string_of_document d^")"
  | Break(s)      -> "Break(\""^String.escaped s^"\")"
  | HardBreak     -> "HardBreak"
  | AlignSpaces i -> "AlignSpaces("^string_of_int i^")"
  | Align d       -> "Align("^string_of_document d^")"
  | Group d       -> "Group("^string_of_document d^")"

let empty =
  { node       = Empty
  ; flat_width = Some 0
  }

let (^^) doc1 doc2 =
  match doc1, doc2 with
    | {node=Empty}, _ -> doc2
    | _, {node=Empty} -> doc1
    | doc1, doc2 ->
      { node       = Append (doc1,doc2)
      ; flat_width = lift2 (+) doc1.flat_width doc2.flat_width
      }

let text s =
  { node       = Text s
  ; flat_width = Some (String.length s) (* FIXME: UTF8 *)
  }

let nest n doc =
  if n = 0 then doc
  else if n < 0 then
    raise (Invalid_argument "Pretty.nest")
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
  ; flat_width = Some (String.length s) (* FIXME: UTF8 *)
  }

let group doc =
  { node       = Group doc
  ; flat_width = doc.flat_width
  }

let alignment_spaces n =
  if n = 0 then empty
  else if n < 0 then
    raise (Invalid_argument "Pretty.alignment_spaces")
  else
    { node       = AlignSpaces n
    ; flat_width = Some 0
    }

let align doc =
  { node       = Align doc
  ; flat_width = doc.flat_width
  }

(******************************************************************************)
let break =
  { node       = Break " "
  ; flat_width = Some 1
  }

let spaces n =
  if n = 0 then empty
  else if n < 0 then
    raise (Invalid_argument "Pretty.spaces")
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

let indent n doc =
  if n < 0 then raise (Invalid_argument "Pretty.indent")
  else spaces n ^^ align doc

(******************************************************************************)
let unit = text "()"

let int i = text (string_of_int i)

let bool = function
  | true  -> text "true"
  | false -> text "false"

let float f = text (string_of_float f)

let string str =
  text "\"" ^^ text (String.escaped str) ^^ text "\""

let char c =
  text "\'" ^^ text (Char.escaped c) ^^ text "\'"

(******************************************************************************)
module type COLLECTIONS = sig
  val delimited :
    left:string ->
    sep:string ->
    right:string ->
    document list ->
    document

  val delimited_array :
    left:string ->
    sep:string ->
    right:string ->
    document array ->
    document

  val associative :
    left:string ->
    map:string ->
    sep:string ->
    right:string ->
    (string * document) list ->
    document

  val list : document list -> document

  val array : (int -> 'a -> document) -> 'a array -> document

  val set : document list -> document

  val tuple : document list -> document

  val constructor : string -> document list -> document
end

module HorizOrVertical = struct
  let delimited ~left ~sep ~right documents =
    let n = String.length left - 1 in
    let sep' =
      break_with ""
      ^^ text sep
      ^^ text " "
      ^^ alignment_spaces n
    in
    align (text left
           ^^ group (alignment_spaces 1
                     ^^ join sep' documents
                     ^^ break_with ""
                     ^^ text right))

  let delimited_array ~left ~sep ~right documents =
    let n = String.length left - 1 in
    let sep' =
      break_with ""
      ^^ text sep
      ^^ text " "
      ^^ alignment_spaces n
    in
    align (text left
           ^^ group (alignment_spaces 1
                     ^^ join_array sep' documents
                     ^^ break_with ""
                     ^^ text right))

  let associative ~left ~map ~sep ~right fields =
    let max_name_width =
      List.fold_left
        (fun n (x,_) -> max n (String.length x)) 0 fields
    in
    let pp_field (fieldname, doc) =
      let spacer = max_name_width - String.length fieldname in
      text fieldname
      ^^ text " "
      ^^ alignment_spaces spacer
      ^^ text map
      ^^ group (nest 2 (break ^^ doc))
    in
    let sep' =
      break_with "" ^^ text sep ^^ text " "
    in
    group (align (text left
                  ^^ text " "
                  ^^ map_join pp_field sep' fields
                  ^^ break
                  ^^ text right))

  let array pp elements =
    delimited_array
      ~left:"[|"
      ~right:"|]"
      ~sep:";"
      (Array.mapi pp elements)

  let list elements =
    delimited
      ~left:"["
      ~sep:";"
      ~right:"]"
      elements

  let set elements =
    delimited
      ~left:"{"
      ~sep:","
      ~right:"}"
      elements

  let tuple elements =
    delimited
      ~left:"("
      ~sep:","
      ~right:")"
      elements

  let constructor name arguments = match arguments with
    | []  -> text name
    | [x] -> group (align (text name ^^ nest 2 (break ^^ x)))
    | xs  -> group (align (text name ^^ nest 2 (break ^^ tuple xs)))
end

module Wrapped = struct
  let delimited ~left ~sep ~right documents =
    text left
    ^^ align (group (wrap (text sep) documents
                     ^^ text right))

  let delimited_array ~left ~sep ~right documents =
    text left
    ^^ align (group (wrap_array (text sep) documents
                     ^^ text right))

  let associative ~left ~map ~sep ~right fields =
    let pp_field (fieldname, doc) =
      text fieldname ^+^ text map ^^ group (nest 2 (break ^^ doc))
    in
    group (text left ^^ text " "
           ^^ align (map_join pp_field (text sep ^^ break) fields
                     ^^ text " " ^^ text right))

  let array pp elements =
    delimited_array
      ~left:"[|"
      ~right:"|]"
      ~sep:";"
      (Array.mapi pp elements)

  let list elements =
    delimited
      ~left:"["
      ~sep:";"
      ~right:"]"
      elements

  let set elements =
    delimited
      ~left:"{"
      ~sep:","
      ~right:"}"
      elements

  let tuple elements =
    delimited
      ~left:"("
      ~sep:","
      ~right:")"
      elements

  let constructor name arguments = match arguments with
    | []  -> text name
    | [x] -> group (align (text name ^^ nest 2 (break ^^ x)))
    | xs  -> group (align (text name ^^ nest 2 (break ^^ tuple xs)))
end

(*
let array pp array =
  let sep = break_with "" ^^ text ", " ^^ alignment_spaces 1 in
  group (align (text "[| "
                ^^ map_join_array pp sep array
                ^^ break
                ^^ text "|]"))

let list elements =
  group (align (text "["
                ^^ alignment_spaces 1
                ^^ join (break_with "" ^^ text "; ") elements
                ^^ break_with ""
                ^^ text "]"))

let set elements =
  group (align (text "{| "
                ^^ join (break_with "" ^^ text ", " ^^ alignment_spaces 1) elements
                ^^ break
                ^^ text "|}"))

let tuple elements =
  group (align (text "("
                ^^ alignment_spaces 1
                ^^ join (break_with "" ^^ text ", ") elements
                ^^ break_with ""
                ^^ text ")"))
(*
  delimited_collection
    ~left:"["
    ~sep:";"
    ~right:"]"
    elements

let set elements =
  delimited_collection
    ~left:"{"
    ~sep:","
    ~right:"}"
    elements

let tuple elements =
  delimited_collection
    ~left:"("
    ~sep:","
    ~right:")"
    elements
*)
*)

(******************************************************************************)
let application head arguments =
  group (align (head ^^ nest 2 (break ^^ join break arguments)))

(******************************************************************************)
type item = int * [`F|`B] * document

let rec fits left : item list -> bool = function
  | _                when left < 0 -> false
  | []                             -> true
  | (i,_, {node=Empty})::z         -> fits left z
  | (i,m, {node=Append (x,y)})::z  -> fits left ((i,m,x)::(i,m,y)::z)
  | (i,_, {node=Text s})::z        -> fits (left-String.length s) z
  | (i,_, {node=Spaces n})::z      -> fits (left-n) z
  | (i,m, {node=Align x})::z       -> fits left ((i,m,x)::z)
  | (i,m, {node=Nest (j,x)})::z    -> fits left ((i,m,x)::z)
  | (i,`F,{node=Break s})::z       -> fits (left-String.length s) z
  | (i,`B,{node=Break _})::z       -> true
  | (i,_, {node=HardBreak})::z     -> true
  | (i,`F,{node=AlignSpaces n})::z -> fits left z
  | (i,`B,{node=AlignSpaces n})::z -> fits (left-n) z
  | (i,_, {node=Group x;flat_width=Some flat_width})::z
                                   -> fits (left-flat_width) z
  | (i,_, {node=Group x})::z       -> fits left ((i,`B,x)::z)

let format output_text output_newline output_spaces width doc =
  let rec process column = function
    | [] ->
      ()

    | (i,m,{node=Empty})::z ->
      process column z

    | (i,m,{node=Append (x,y)})::z ->
      process column ((i,m,x)::(i,m,y)::z)

    | (i,m,{node=Text s})::z ->
      output_text s;
      process (column + String.length s) z

    | (i,m,{node=Spaces n})::z ->
      output_spaces n;
      process (column + n) z

    | (i,m,{node=Align x})::z ->
      process column ((column,m,x)::z)

    | (i,m,{node=Nest (j,x)})::z ->
      process column ((i+j,m,x)::z)

    | (i,`F,{node=Break s})::z ->
      output_text s;
      process (column + String.length s) z

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
      process column ((i,`F,x)::z)

    | (i,_,{node=Group x})::z ->
      process column ((i,`B,x)::z)
  in
  process 0 [(0,`B,group doc)]

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

let custom_format ?(width=80)
    ~output_text ~output_newline ~output_spaces doc =
  format
    output_text
    output_newline
    output_spaces
    width
    doc
