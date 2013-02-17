open OUnit
open Pretty

(* FIXME: do UTF8 *)
let printable_string () =
  let length = Random.int 100 in
  let buffer = Buffer.create length in
  for i = 1 to length do
    Buffer.add_char buffer (Char.chr (Random.int (128 - 32) + 32))
  done;
  Buffer.contents buffer

type document_context =
  | Hole
  | Append1 of document_context * document
  | Append2 of document * document_context
  | Nest    of int * document_context
  | Group   of document_context
  | Align   of document_context

let rec plug document = function
  | Hole                -> document
  | Append1 (ctxt, doc) -> plug document ctxt ^^ doc
  | Append2 (doc, ctxt) -> doc ^^ plug document ctxt
  | Group   ctxt        -> group (plug document ctxt)
  | Nest    (i,ctxt)    -> nest i (plug document ctxt)
  | Align   ctxt        -> align (plug document ctxt)

let random_document_of_depth depth =
  let rec make depth =
    if depth = 0 then
      if Random.bool () then empty else text (printable_string ())
    else match Random.int 9 with
      | 0 -> empty
      | 1 -> make (depth-1) ^^ make (depth-1)
      | 2 -> text (printable_string ())
      | 3 -> nest (Random.int 10) (make (depth-1))
      | 4 -> break (* with? *)
      | 5 -> hardbreak
      | 6 -> alignSpc (Random.int 20)
      | 7 -> group (make (depth-1))
      | 8 -> align (make (depth-1))
      | _ -> assert false
  in
  make depth

let document_context () =
  let rec make depth =
    if depth = 0 then Hole
    else match Random.int 5 with
      | 0 -> Append1 (make (depth-1), random_document_of_depth (depth-1))
      | 1 -> Append2 (random_document_of_depth (depth-1), make (depth-1))
      | 2 -> Nest (Random.int 10, make (depth-1))
      | 3 -> Group (make (depth-1))
      | 4 -> Align (make (depth-1))
      | _ -> assert false
  in
  make (Random.int 10)

let document () = 
  random_document_of_depth (Random.int 7)

let int_range minimum maximum () =
  Random.int (maximum - minimum + 1) + minimum

let forall domain predicate = fun () ->
  predicate (domain ()) ()

let check_property property =
  for i = 1 to 1000 do
    property ()
  done

let (^$) f x = f x

(******************************************************************************)
let ( =~= ) document1 document2 =
  forall (int_range 2 200) ^$ fun width ->
    forall document_context ^$ fun c ->
      fun () ->
        assert_equal
          (to_string ~width (plug document1 c))
          (to_string ~width (plug document2 c))

let check_render ~width ~document ~expected_output =
  assert_equal ~printer:(String.escaped)
    expected_output
    (Pretty.to_string ~width document)

(******************************************************************************)
let prop_empty_left_unit () =
  check_property ^$
    forall document ^$ fun d ->
      d =~= empty ^^ d

let prop_empty_right_unit () =
  check_property ^$
    forall document ^$ fun d ->
      d =~= d ^^ empty

let prop_append_assoc () =
  check_property ^$
    forall document ^$ fun d1 ->
      forall document ^$ fun d2 ->
        forall document ^$ fun d3 ->
          d1 ^^ (d2 ^^ d3) =~= (d1 ^^ d2) ^^ d3

let prop_text_append () =
  check_property ^$
    forall printable_string ^$ fun s1 ->
      forall printable_string ^$ fun s2 ->
        text s1 ^^ text s2 =~= text (s1 ^ s2)

let prop_text_empty () =
  check_property ^$
    (text "" =~= empty)

let prop_text_group () =
  check_property ^$
    forall document ^$ fun d ->
      forall printable_string ^$ fun s ->
        group (text s ^^ d) =~= text s ^^ group d

let prop_nest_group () =
  check_property ^$
    forall document ^$ fun d ->
      forall (int_range 0 12) ^$ fun n ->
        group (nest n d) =~= nest n (group d)

let prop_align_group () =
  check_property ^$
    forall document ^$ fun d ->
      group (align d) =~= align (group d)

let prop_zero_nest () =
  check_property ^$
    forall document ^$ fun d ->
      nest 0 d =~= d

let prop_nest_nest () =
  check_property ^$
    forall (int_range 0 12) ^$ fun n1 ->
      forall (int_range 0 12) ^$ fun n2 ->
        forall document ^$ fun d ->
          nest n1 (nest n2 d) =~= nest (n1 + n2) d

let prop_nest_text () =
  check_property ^$
    forall document ^$ fun d ->
      forall printable_string ^$ fun s ->
        forall (int_range 0 12) ^$ fun n ->
          text s ^^ nest n d =~= nest n (text s ^^ d)  

let prop_nest_alignSpc () =
  check_property ^$
    forall document ^$ fun d ->
      forall (int_range 0 40) ^$ fun n1 ->
        forall (int_range 0 40) ^$ fun n2 ->
          alignSpc n1 ^^ nest n2 d =~= nest n2 (alignSpc n1 ^^ d)

let prop_align_text () =
  check_property ^$
    forall document ^$ fun d ->
      forall printable_string ^$ fun s ->
        align (text s ^^ nest (String.length s) d) =~= text s ^^ align d

let prop_align_nest () =
  check_property ^$
    forall document ^$ fun d ->
      forall (int_range 1 12) ^$ fun n ->
        nest n (align d) =~= align d

let prop_group_empty () =
  check_property ^$
    (group empty =~= empty)

let prop_nest_empty () =
  check_property ^$
    forall (int_range 0 12) ^$ fun n ->
      nest n empty =~= empty

let prop_align_empty () =
  check_property ^$
    (align empty =~= empty)

let prop_group_group () =
  check_property ^$
    forall document ^$ fun d ->
      group (group d) =~= group d

let prop_group_hardbreak () =
  check_property ^$
    forall document_context ^$ fun c ->
      group (plug hardbreak c) =~= plug hardbreak c

(******************************************************************************)
(* Check the derived combinators *)
let prop_break_with () =
  check_property ^$
    (break =~= breakWith " ")

(* FIXME: and the rest... *)

(******************************************************************************)
(* Rendering tests *)
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

let test_nest () =
  check_render
    ~width:3
    ~document:(text "xxx" ^^ nest 2 (break ^^ text "yyy"))
    ~expected_output:"xxx\n  yyy"

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

let test_breakWith1 () =
  check_render
    ~width:10
    ~document:(text "xxx" ^^ breakWith "---" ^^ text "yyy")
    ~expected_output:"xxx---yyy"

let test_breakWith2 () =
  check_render
    ~width:8
    ~document:(text "xxx" ^^ breakWith "---" ^^ text "yyy")
    ~expected_output:"xxx\nyyy"

let test_alignSpc1 () =
  check_render
    ~width:7
    ~document:(text "xxx" ^^ alignSpc 3 ^^ text "=" ^^ break ^^ text "yyy")
    ~expected_output:"xxx   =\nyyy"

let test_alignSpc2 () =
  check_render
    ~width:8
    ~document:(text "xxx" ^^ alignSpc 3 ^^ text "=" ^^ break ^^ text "yyy")
    ~expected_output:"xxx= yyy"

(* FIXME: and more... *)

(******************************************************************************)
let suite =
  "pretty-monospace tests" >:::
    [ "rendering tests" >:::
      [ "text"            >:: test_text
      ; "append"          >:: test_append
      ; "empty"           >:: test_empty
      ; "hardbreak"       >:: test_hardbreak
      ; "nest"            >:: test_nest
      ; "align1"          >:: test_align1
      ; "align2"          >:: test_align2
      ; "group1"          >:: test_group1
      ; "group2"          >:: test_group2
      ; "breakWith1"      >:: test_breakWith1
      ; "breakWith2"      >:: test_breakWith2
      ; "alignSpc1"       >:: test_alignSpc1
      ; "alignSpc2"       >:: test_alignSpc2
      ]

    ; "combinator properties" >:::
      [ "empty_left_unit"   >:: prop_empty_left_unit
      ; "empty_right_unit"  >:: prop_empty_right_unit
      ; "append_assoc"      >:: prop_append_assoc

      ; "text_append"       >:: prop_text_append
      ; "text_empty"        >:: prop_text_empty

      ; "nest_text"         >:: prop_nest_text
      ; "nest_empty"        >:: prop_nest_empty
      ; "nest_group"        >:: prop_nest_group
      ; "zero_nest"         >:: prop_zero_nest
      ; "nest_nest"         >:: prop_nest_nest
      ; "nest_alignSpc"     >:: prop_nest_alignSpc

      ; "align_empty"       >:: prop_align_empty
      ; "align_text"        >:: prop_align_text
      ; "align_group"       >:: prop_align_group
      ; "align_nest"        >:: prop_align_nest

      ; "text_group"        >:: prop_text_group
      ; "group_empty"       >:: prop_group_empty
      ; "group_group"       >:: prop_group_group
      ; "group_hardbreak"   >:: prop_group_hardbreak
      ]

    ; "derived combinator properties" >:::
      [ "break_with"        >:: prop_break_with
      ]
    ]

let _ =
  Random.self_init ();
  run_test_tt_main suite

