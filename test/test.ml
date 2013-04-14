(* (C) Robert Atkey 2013, see LICENSE for more information. *)

open Pretty

open OUnit
open CamlCheck.PrimitiveDomains
open CamlCheck.Property

(******************************************************************************)
let intersperse separator xs =
  let rec loop acc = function
    | []    -> List.rev acc
    | x::xs ->
      match acc with
        | [] -> loop [x] xs
        | _  -> loop (x::separator::acc) xs
  in loop [] xs

(******************************************************************************)
(* A domain of documents, suitable for randomised testing *)

let document_generator depth =
  let open CamlCheck.Generator in
  let open CamlCheck.Domain in
  let rec make depth =
    if depth = 0 then
      bind (int 4) begin function
        | 0 -> return empty
        | 1 -> lift text (generator printable_ascii_string)
        | 2 -> return break (* with? *)
        | 3 -> return hardbreak
        | _ -> assert false
      end
    else
      bind (int 5) begin function
        | 0 -> lift2 (^^) (make (depth-1)) (make (depth-1))
        | 1 -> lift2 nest (int 10) (make (depth-1))
        | 2 -> lift alignment_spaces (int 20)
        | 3 -> lift group (make (depth-1))
        | 4 -> lift align (make (depth-1))
        | _ -> assert false
      end
  in
  make depth

let document =
  let open CamlCheck.Generator in
  CamlCheck.Domain.make
    ~generate:(bind (int 10) document_generator)
    ~to_string:string_of_document
    ~description:"document"
    ()

(******************************************************************************)
type document_context =
  | Hole
  | Append1 of document_context * document
  | Append2 of document * document_context
  | Nest    of int * document_context
  | Group   of document_context
  | Align   of document_context

let rec string_of_document_context = function
  | Hole         -> "Hole"
  | Append1(c,d) -> "Append1(" ^ string_of_document_context c ^ ", " ^ string_of_document d ^ ")"
  | Append2(d,c) -> "Append2(" ^ string_of_document d ^ ", " ^ string_of_document_context c ^ ")"
  | Nest(n,c)    -> "Nest(" ^ string_of_int n ^ ", " ^ string_of_document_context c ^ ")"
  | Group c      -> "Group(" ^ string_of_document_context c ^ ")"
  | Align c      -> "Align(" ^ string_of_document_context c ^ ")"

let (+//) context document =
  let rec plug = function
    | Hole                   -> document
    | Append1 (context, doc) -> plug context ^^ doc
    | Append2 (doc, context) -> doc ^^ plug context
    | Group   context        -> group (plug context)
    | Nest    (i,context)    -> nest i (plug context)
    | Align   context        -> align (plug context)
  in
  plug context

let append1 x y = Append1 (x,y)
let append2 x y = Append2 (x,y)
let nest_ctxt n x = Nest (n,x)
let group_ctxt x = Group x
let align_ctxt x = Align x

let document_context =
  let open CamlCheck.Generator in
  let rec make depth =
    if depth = 0 then return Hole
    else
      bind (int 5) begin function
        | 0 -> lift2 append1 (make (depth-1)) (document_generator (depth-1))
        | 1 -> lift2 append2 (document_generator (depth-1)) (make (depth-1))
        | 2 -> lift2 nest_ctxt (int 10) (make (depth-1))
        | 3 -> lift group_ctxt (make (depth-1))
        | 4 -> lift align_ctxt (make (depth-1))
        | _ -> assert false
      end
  in
  let rec shrink = function
    | Hole -> []
    | Append1 (c,d) -> c :: List.map (fun c' -> Append1 (c',d)) (shrink c)
    | Append2 (d,c) -> c :: List.map (fun c' -> Append2 (d,c')) (shrink c)
    | Nest (n,c) -> c :: if n > 0 then [Nest (n-1,c)] else []
    | Group c -> c :: List.map (fun c' -> Group c') (shrink c)
    | Align c -> c :: List.map (fun c' -> Align c') (shrink c)
  in
  CamlCheck.Domain.make
    ~generate:(bind (int 10) make)
    ~to_string:string_of_document_context
    ~description:"document_context"
    ~shrink
    ()

(******************************************************************************)
(** Property that states that two documents are contextually equivalent. *)
let ( =~= ) document1 document2 =
  forall (int_range 0 200) ^$ fun width ->
    forall document_context ^$ fun c ->
      check_equal ~to_string:String.escaped
        (render ~width (c +// document1))
        (render ~width (c +// document2))

(******************************************************************************)
(* Combinator properties *)

let prop_empty_left_unit =
  forall document ^$ fun d ->
    d =~= empty ^^ d

let prop_empty_right_unit =
  forall document ^$ fun d ->
    d =~= d ^^ empty

let prop_append_assoc =
  forall document ^$ fun d1 ->
    forall document ^$ fun d2 ->
      forall document ^$ fun d3 ->
        d1 ^^ (d2 ^^ d3) =~= (d1 ^^ d2) ^^ d3

let prop_text_append =
  forall printable_ascii_string ^$ fun s1 ->
    forall printable_ascii_string ^$ fun s2 ->
      text s1 ^^ text s2 =~= text (s1 ^ s2)

let prop_text_empty =
  text "" =~= empty

let prop_text_group =
  forall document ^$ fun d ->
    forall printable_ascii_string ^$ fun s ->
      group (text s ^^ d) =~= text s ^^ group d

let prop_nest_group =
  forall document ^$ fun d ->
    forall (int_range 0 12) ^$ fun n ->
      group (nest n d) =~= nest n (group d)

let prop_align_group =
  forall document ^$ fun d ->
    group (align d) =~= align (group d)

let prop_zero_nest =
  forall document ^$ fun d ->
    nest 0 d =~= d

let prop_nest_nest =
  forall (int_range 0 12) ^$ fun n1 ->
    forall (int_range 0 12) ^$ fun n2 ->
      forall document ^$ fun d ->
        nest n1 (nest n2 d) =~= nest (n1 + n2) d

let prop_nest_text =
  forall document ^$ fun d ->
    forall printable_ascii_string ^$ fun s ->
      forall (int_range 0 12) ^$ fun n ->
        text s ^^ nest n d =~= nest n (text s ^^ d)

let prop_nest_alignment_spaces =
  forall document ^$ fun d ->
    forall (int_range 0 40) ^$ fun n1 ->
      forall (int_range 0 40) ^$ fun n2 ->
        alignment_spaces n1 ^^ nest n2 d =~= nest n2 (alignment_spaces n1 ^^ d)

let prop_alignment_spaces_empty =
  alignment_spaces 0 =~= empty

let prop_align_text =
  forall document ^$ fun d ->
    forall printable_ascii_string ^$ fun s ->
      align (text s ^^ nest (String.length s) d) =~= text s ^^ align d

let prop_align_nest =
  forall document ^$ fun d ->
    forall (int_range 1 12) ^$ fun n ->
      nest n (align d) =~= align d

let prop_align_align =
  forall document ^$ fun d ->
    align (align d) =~= align d

let prop_group_empty =
  group empty =~= empty

let prop_nest_empty =
  forall (int_range 0 12) ^$ fun n ->
    nest n empty =~= empty

let prop_align_empty =
  align empty =~= empty

let prop_group_group =
  forall document ^$ fun d ->
    group (group d) =~= group d

let prop_group_hardbreak =
  forall document_context ^$ fun c ->
    group (c +// hardbreak) =~= c +// hardbreak

(******************************************************************************)
(* Check the derived combinators *)
let prop_break_with =
  break =~= break_with " "

let prop_spaces =
  forall (int_range 0 20) ^$ fun n ->
    spaces n =~= text (String.make n ' ')

let prop_space =
  space =~= text " "

let prop_append_space =
  forall document ^$ fun d1 ->
    forall document ^$ fun d2 ->
      d1 ^^ text " " ^^ d2 =~= d1 ^+^ d2

let prop_append_break =
  forall document ^$ fun d1 ->
    forall document ^$ fun d2 ->
      d1 ^^ break ^^ d2 =~= d1 ^/^ d2

let prop_append_hardbreak =
  forall document ^$ fun d1 ->
    forall document ^$ fun d2 ->
      d1 ^^ hardbreak ^^ d2 =~= d1 ^//^ d2

let prop_join =
  forall (list document) ^$ fun ds ->
    forall document ^$ fun d ->
      join d ds =~= List.fold_left ( ^^ ) empty (intersperse d ds)

let prop_map_join =
  forall (list document) ^$ fun ds ->
    forall document ^$ fun d ->
      forall document ^$ fun d' ->
        let f x = x ^^ d' in
        map_join f d ds =~=
        List.fold_left ( ^^ ) empty (intersperse d (List.map f ds))

let prop_join_array =
  forall (list document) ^$ fun ds ->
    forall document ^$ fun d ->
      join_array d (Array.of_list ds)
      =~=
      List.fold_left ( ^^ ) empty (intersperse d ds)

let prop_map_join_array =
  forall (list document) ^$ fun ds ->
    forall document ^$ fun d ->
      let f i x = x in
      map_join_array f d (Array.of_list ds)
      =~=
      List.fold_left ( ^^ ) empty (intersperse d ds)

let prop_int =
  forall (int_range (-1000) 1000) ^$ fun i ->
    text (string_of_int i) =~= Pretty.int i

let prop_bool =
  forall bool ^$ fun b ->
    text (if b then "true" else "false") =~= Pretty.bool b

let prop_unit =
  text "()" =~= Pretty.unit

let prop_string =
  forall string ^$ fun s ->
    text ("\"" ^ String.escaped s ^ "\"") =~= Pretty.string s

let prop_char =
  forall char ^$ fun c ->
    text ("\'" ^ Char.escaped c ^ "\'") =~= Pretty.char c

let prop_float =
  forall (float_range (-2e30) (2e30)) ^$ fun f ->
    text (string_of_float f) =~= Pretty.float f

let prop_wrap =
  forall document ^$ fun sep ->
    forall (list document) ^$ fun ds ->
      match ds with
        | [] -> wrap sep ds =~= empty
        | d::ds ->
          wrap sep (d::ds) =~=
          join sep (d :: List.map (fun x -> group (break ^^ x)) ds)

let prop_wrap_array =
  forall document ^$ fun sep ->
    forall (list document) ^$ fun ds ->
      wrap sep ds =~= wrap_array sep (Array.of_list ds)

let prop_indent =
  forall document ^$ fun d ->
    forall (int_range 0 20) ^$ fun n ->
      indent n d =~= spaces n ^^ align d

(******************************************************************************)
let prop_list =
  forall (list document) ^$ fun ds ->
    Pretty.HorizOrVertical.list ds =~=
    group (align (text "["
                  ^^ alignment_spaces 1
                  ^^ join (break_with "" ^^ text "; ") ds
                  ^^ break_with ""
                  ^^ text "]"))

let prop_tuple =
  forall (list document) ^$ fun ds ->
    Pretty.HorizOrVertical.tuple ds =~=
    align (text "("
           ^^ group (alignment_spaces 1
                     ^^ join (break_with "" ^^ text ", ") ds
                     ^^ break_with ""
                     ^^ text ")"))

let prop_application =
  forall document ^$ fun d ->
    forall (list document) ^$ fun ds ->
      application d ds =~=
      group (align (d ^^ nest 2 (break ^^ join break ds)))

(* FIXME: and the rest... *)

(******************************************************************************)
(* Properties of the combinators on bad input *)

let prop_exn_nest =
  forall document ^$ fun d ->
    forall (int_range (-100) (-1)) ^$ fun n ->
      check_raises
        (Invalid_argument "Pretty.nest")
        (fun () -> nest n d)

let prop_exn_alignment_spaces =
  forall (int_range (-100) (-1)) ^$ fun n ->
    check_raises
      (Invalid_argument "Pretty.alignment_spaces")
      (fun () -> alignment_spaces n)

let prop_exn_spaces =
  forall (int_range (-100) (-1)) ^$ fun n ->
    check_raises
      (Invalid_argument "Pretty.spaces")
      (fun () -> spaces n)

let prop_exn_indent =
  forall document ^$ fun d ->
    forall (int_range (-100) (-1)) ^$ fun n ->
      check_raises
        (Invalid_argument "Pretty.indent")
        (fun () -> indent n d)

(******************************************************************************)
(* Rendering tests *)

let check_render ~width ~document ~expected_output =
  assert_equal ~printer:String.escaped
    expected_output
    (render ~width document)

let test_text () =
  check_render
    ~width:80
    ~document:(text "xxx")
    ~expected_output:"xxx"

let test_append () =
  check_render
    ~width:80
    ~document:(text "xxx" ^^ text "yyy")
    ~expected_output:"xxxyyy"

let test_empty () =
  check_render
    ~width:80
    ~document:empty
    ~expected_output:""

let test_hardbreak () =
  check_render
    ~width:80
    ~document:(text "xxx" ^^ hardbreak ^^ text "yyy")
    ~expected_output:"xxx\nyyy"

let test_break1 () =
  check_render
    ~width:80
    ~document:(text "xxx" ^^ break ^^ text "yyy")
    ~expected_output:"xxx yyy"

let test_break2 () =
  check_render
    ~width:2
    ~document:(text "xxx" ^^ break ^^ text "yyy")
    ~expected_output:"xxx\nyyy"

let test_nest1 () =
  check_render
    ~width:3
    ~document:(text "xxx" ^^ nest 2 (break ^^ text "yyy"))
    ~expected_output:"xxx\n  yyy"

let test_nest2 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ nest 2 (break ^^ text "yyy"))
    ~expected_output:"xxx yyy"

let test_nest3 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ nest 3 (group (break ^^ text "yyy")) ^/^ text "zzzz")
    ~expected_output:"xxx yyy\nzzzz"

let test_nest4 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ nest 3 (group (break ^^ text "yyy") ^/^ text "zzzz"))
    ~expected_output:"xxx yyy\n   zzzz"

let test_align1 () =
  check_render
    ~width:9
    ~document:(text "xxx" ^^ align (text "yyy" ^^ break ^^ text "zzz"))
    ~expected_output:"xxxyyy\n   zzz"

let test_align2 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ align (text "yyy" ^^ break ^^ text "zzz"))
    ~expected_output:"xxxyyy zzz"

let test_group1 () =
  check_render
    ~width:10
    ~document:(group (text "xxx" ^/^ text "yyy") ^/^ text "zzzzzzzzzz")
    ~expected_output:"xxx yyy\nzzzzzzzzzz"

let test_group2 () =
  check_render
    ~width:10
    ~document:(text "xxx"
               ^^ group (break ^^ group (text "yyy" ^^ break ^^ text "zzzz")))
    ~expected_output:"xxx\nyyy zzzz"

let test_break_with1 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ break_with "---" ^^ text "yyy")
    ~expected_output:"xxx---yyy"

let test_break_with2 () =
  check_render
    ~width:8
    ~document:(text "xxx" ^^ break_with "---" ^^ text "yyy")
    ~expected_output:"xxx\nyyy"

let test_alignment_spaces1 () =
  check_render
    ~width:7
    ~document:(text "xxx" ^^ alignment_spaces 3 ^^ text "=" ^^ break ^^ text "yyy")
    ~expected_output:"xxx   =\nyyy"

let test_alignment_spaces2 () =
  check_render
    ~width:8
    ~document:(text "xxx" ^^ alignment_spaces 3 ^^ text "=" ^^ break ^^ text "yyy")
    ~expected_output:"xxx= yyy"

let test_indent () =
  check_render
    ~width:8
    ~document:(text "xxx" ^^ indent 3 (text "yyy" ^/^ text "zzz"))
    ~expected_output:"xxx   yyy\n      zzz"

(* These three tests test that the breaking computation takes into
   account all of the queued print jobs on the current line *)
let test_lineleft_doc =
  text "begin"
  ^^ nest 3 (break
             ^^ group (text "stmt;"
                       ^/^ text "stmt;"
                       ^/^ text "stmt;"))
  ^^ text "end"

let test_lineleft1 () =
  check_render
    ~width:10
    ~document:test_lineleft_doc
    ~expected_output:"begin\n   stmt;\n   stmt;\n   stmt;end"

let test_lineleft2 () =
  check_render
    ~width:20
    ~document:test_lineleft_doc
    ~expected_output:"begin\n   stmt;\n   stmt;\n   stmt;end"

let test_lineleft3 () =
  check_render
    ~width:30
    ~document:test_lineleft_doc
    ~expected_output:"begin stmt; stmt; stmt;end"

(******************************************************************************)
(* Tests to check that the output functions agree with each other *)
let prop_custom_output =
  forall document ^$ fun d ->
    forall (int_range 1 100) ^$ fun n ->
      let b = Buffer.create 8192 in
      custom_format ~width:n
        ~output_text:(Buffer.add_string b)
        ~output_newline:(fun () -> Buffer.add_char b '\n')
        ~output_spaces:(fun n -> Buffer.add_string b (String.make n ' '))
        d;
      check_equal
        ~to_string:String.escaped
        (Buffer.contents b)
        (render ~width:n d)

(******************************************************************************)
let suite =
  "pretty-monospace tests" >:::
    [ "rendering tests" >:::
      [ "text"            >:: test_text
      ; "append"          >:: test_append
      ; "empty"           >:: test_empty
      ; "break1"          >:: test_break1
      ; "break2"          >:: test_break2
      ; "hardbreak"       >:: test_hardbreak
      ; "nest1"           >:: test_nest1
      ; "nest2"           >:: test_nest2
      ; "nest3"           >:: test_nest3
      ; "nest4"           >:: test_nest4
      ; "align1"          >:: test_align1
      ; "align2"          >:: test_align2
      ; "group1"          >:: test_group1
      ; "group2"          >:: test_group2
      ; "break_with1"     >:: test_break_with1
      ; "break_with2"     >:: test_break_with2
      ; "alignment_spaces1">:: test_alignment_spaces1
      ; "alignment_spaces2">:: test_alignment_spaces2
      ; "indent"          >:: test_indent
      ; "lineleft1"       >:: test_lineleft1
      ; "lineleft2"       >:: test_lineleft2
      ; "lineleft3"       >:: test_lineleft3
      ]

    ; "exception tests" >:::
      [ "nest"             >:: test_of_property prop_exn_nest
      ; "alignment_spaces" >:: test_of_property prop_exn_alignment_spaces
      ; "spaces"           >:: test_of_property prop_exn_spaces
      ; "indent"           >:: test_of_property prop_exn_indent
      ]

    ; "combinator properties" >:::
      [ "empty_left_unit"   >:: test_of_property prop_empty_left_unit
      ; "empty_right_unit"  >:: test_of_property prop_empty_right_unit
      ; "append_assoc"      >:: test_of_property prop_append_assoc

      ; "text_append"       >:: test_of_property prop_text_append
      ; "text_empty"        >:: test_of_property prop_text_empty

      ; "nest_text"         >:: test_of_property prop_nest_text
      ; "nest_empty"        >:: test_of_property prop_nest_empty
      ; "nest_group"        >:: test_of_property prop_nest_group
      ; "zero_nest"         >:: test_of_property prop_zero_nest
      ; "nest_nest"         >:: test_of_property prop_nest_nest
      ; "nest_alignment_spaces">:: test_of_property prop_nest_alignment_spaces

      ; "alignment_spaces_empty">:: test_of_property prop_alignment_spaces_empty

      ; "align_empty"       >:: test_of_property prop_align_empty
      ; "align_text"        >:: test_of_property prop_align_text
      ; "align_group"       >:: test_of_property prop_align_group
      ; "align_nest"        >:: test_of_property prop_align_nest
      ; "align_align"       >:: test_of_property prop_align_align

      ; "text_group"        >:: test_of_property prop_text_group
      ; "group_empty"       >:: test_of_property prop_group_empty
      ; "group_group"       >:: test_of_property prop_group_group
      ; "group_hardbreak"   >:: test_of_property prop_group_hardbreak
      ]

    ; "derived combinator properties" >:::
      [ "break_with"        >:: test_of_property prop_break_with
      ; "spaces"            >:: test_of_property prop_spaces
      ; "space"             >:: test_of_property prop_space
      ; "append_space"      >:: test_of_property prop_append_space
      ; "append_break"      >:: test_of_property prop_append_break
      ; "append_hardbreak"  >:: test_of_property prop_append_hardbreak
      ; "join"              >:: test_of_property prop_join
      ; "map_join"          >:: test_of_property prop_map_join
      ; "join_array"        >:: test_of_property prop_join_array
      ; "map_join_array"    >:: test_of_property prop_map_join_array
      ; "int"               >:: test_of_property prop_int
      ; "bool"              >:: test_of_property prop_bool
      ; "unit"              >:: test_of_property prop_unit
      ; "string"            >:: test_of_property prop_string
      ; "char"              >:: test_of_property prop_char
      ; "float"             >:: test_of_property prop_float
      ; "list"              >:: test_of_property prop_list
      ; "tuple"             >:: test_of_property prop_tuple
      ; "application"       >:: test_of_property prop_application
      ; "wrap"              >:: test_of_property prop_wrap
      ; "wrap_array"        >:: test_of_property prop_wrap_array
      ; "indent"            >:: test_of_property prop_indent
      ]

    ; "output" >:::
      [ "custom_output"     >:: test_of_property prop_custom_output
      ]

    ]

let _ =
  run_test_tt_main suite
