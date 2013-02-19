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
  | Nest     of int * document (** increment the indentation level *)
  | Break    of string (** the string if the containing group is formatted flat, a newline otherwise *)
  | HardBreak (** a mandatory line break *)
  | AlignSpaces of int (** 'n' spaces if the containing group is formatted with line breaks, nothing otherwise *)
  | Align    of document (** set the indentation level to the current column *)
  | Group    of document (** group a sub-document for deciding whether to break lines or not *)

let empty =
  { node       = Empty
  ; flat_width = Some 0
  }

let (^^) doc1 doc2 =
  { node       = Append (doc1,doc2)
  ; flat_width = lift2 (+) doc1.flat_width doc2.flat_width
  }

let text s =
  { node       = Text s
  ; flat_width = Some (String.length s) (* FIXME: UTF8 *)
  }

let nest i doc =
  { node       = Nest (i, doc)
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

let (^+^) x y = x ^^ text " " ^^ y

let (^/^) x y = x ^^ break ^^ y

let (^//^) x y = x ^^ hardbreak ^^ y

let concat sep list =
  let i = ref 0 in
  List.fold_left
    (fun doc x ->
      let doc' = if !i = 0 then x else doc ^^ sep ^^ x
      in incr i; doc')
    empty
    list

let map_concat_array pp sep array =
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

let record fields =
  let max_name_width =
    List.fold_left max 0 (List.map String.length (List.map fst fields))
  in
  let pp_field (fieldname, doc) =
    let spacer = max_name_width - String.length fieldname in
    text fieldname
    ^^ text " "
    ^^ alignment_spaces spacer
    ^^ text "="
    ^^ group (nest 2 (break ^^ doc))
  in
  group (align (text "{ "
                ^^ concat (break_with "" ^^ text "; ")
                          (List.map pp_field fields)
                ^^ break
                ^^ text "}"))

let array pp array =
  let sep = break_with "" ^^ text ", " ^^ alignment_spaces 1 in
  group (align (text "[| "
                ^^ map_concat_array pp sep array
                ^^ break
                ^^ text "|]"))

let list elements =
  group (align (text "["
                ^^ alignment_spaces 1
                ^^ concat (break_with "" ^^ text "; ") elements
                ^^ break_with ""
                ^^ text "]"))

let set elements =
  group (align (text "{| "
                ^^ concat (break_with "" ^^ text ", " ^^ alignment_spaces 1) elements
                ^^ break
                ^^ text "|}"))

let tuple elements =
  group (align (text "("
                ^^ alignment_spaces 1
                ^^ concat (break_with "" ^^ text ", ") elements
                ^^ break_with ""
                ^^ text ")"))

let application head arguments =
  group (align (head ^^ nest 2 (break ^^ concat break arguments)))

let constructor name arguments = match arguments with
  | []  -> text name
  | [x] -> group (align (text name ^^ nest 2 (break ^^ x)))
  | xs  -> group (align (text name ^^ nest 2 (break ^^ tuple xs)))

(******************************************************************************)
let format output_text output_newline output_spaces width doc =
  let rec process column = function
    | [] ->
      ()

    | (i,m,{node=Empty;_})::z ->
      process column z

    | (i,m,{node=Append (x,y);_})::z ->
      process column ((i,m,x)::(i,m,y)::z)

    | (i,m,{node=Text s;_})::z ->
      output_text s;
      process (column + String.length s) z

    | (i,m,{node=Align x;_})::z ->
      process column ((column,m,x)::z)

    | (i,m,{node=Nest (j,x);_})::z ->
      process column ((i+j,m,x)::z)

    | (i,`F,{node=Break s;_})::z ->
      output_text s;
      process (column + String.length s) z

    | (i,`B,{node=Break s;_})::z -> 
      output_newline ();
      output_spaces i;
      process i z

    | (i,`F,{node=HardBreak;_})::z ->
      assert false

    | (i,`B,{node=HardBreak;_})::z ->
      output_newline ();
      output_spaces i;
      process i z

    | (i,`F,{node=AlignSpaces n;_})::z ->
      process column z

    | (i,`B,{node=AlignSpaces n;_})::z ->
      output_spaces n;
      process (column + n) z

    | (i,_,{node=Group x;flat_width=Some flat_width})::z
        when flat_width <= width-column ->
      process column ((i,`F,x)::z)

    | (i,_,{node=Group x;_})::z ->
      process column ((i,`B,x)::z)
  in
  process 0 [(0,`F,group doc)]

(******************************************************************************)
let print ?(width=80) doc =
  format
    print_string
    print_newline
    (fun n -> print_string (String.make n ' '))
    width
    doc

let prerr ?(width=80) doc =
  format
    prerr_string
    prerr_newline
    (fun n -> prerr_string (String.make n ' '))
    width
    doc

let output ?(width=80) ch doc =
  format
    (fun s  -> output_string ch s)
    (fun () -> output_char ch '\n')
    (fun n  -> output_string ch (String.make n ' '))
    width
    doc

let to_string ?(width=80) doc =
  let b = Buffer.create 2048 in
  format
    (fun s  -> Buffer.add_string b s)
    (fun () -> Buffer.add_char b '\n')
    (fun n  -> Buffer.add_string b (String.make n ' '))
    width
    doc;
  Buffer.contents b

let custom_output ?(width=80)
    ~output_text ~output_newline ~output_spaces doc =
  format
    output_text
    output_newline
    output_spaces
    width
    doc
