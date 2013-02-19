open OUnit
open Pretty

let intersperse separator xs =
  let rec loop acc = function
    | []    -> List.rev acc
    | x::xs ->
      (match acc with
        | [] -> loop [x] xs
        | _  -> loop (x::separator::acc) xs)
  in loop [] xs

let bool () =
  Random.bool ()

let float_range minimum maximum () =
  (* FIXME: this is wrong... *)
  Random.float (maximum -. minimum) +. minimum

let list gen () =
  let length = Random.int 20 in
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (gen () :: acc) (n-1)
  in
  loop [] length

let printable_char () =
  Char.chr (Random.int (128 - 32) + 32)

let char () =
  Char.chr (Random.int 256)

(* FIXME: do UTF8 *)
let printable_string () =
  let length = Random.int 100 in
  let str    = String.create length in
  for i = 0 to length - 1 do
    str.[i] <- printable_char ()
  done;
  str

let string () =
  let length = Random.int 100 in
  let str    = String.create length in
  for i = 0 to length - 1 do
    str.[i] <- char ()
  done;
  str

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
      | 6 -> alignment_spaces (Random.int 20)
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
  random_document_of_depth (Random.int 10)

let int_range minimum maximum () =
  (* FIXME: overflow *)
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

let prop_nest_alignment_spaces () =
  check_property ^$
    forall document ^$ fun d ->
      forall (int_range 0 40) ^$ fun n1 ->
        forall (int_range 0 40) ^$ fun n2 ->
          alignment_spaces n1 ^^ nest n2 d =~= nest n2 (alignment_spaces n1 ^^ d)

let prop_alignment_spaces_empty () =
  check_property ^$
    (alignment_spaces 0 =~= empty)

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

let prop_align_align () =
  check_property ^$
    forall document ^$ fun d ->
      align (align d) =~= align d

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
    (break =~= break_with " ")

let prop_append_space () =
  check_property ^$
    forall document ^$ fun d1 ->
      forall document ^$ fun d2 ->
        d1 ^^ text " " ^^ d2 =~= d1 ^+^ d2

let prop_append_break () =
  check_property ^$
    forall document ^$ fun d1 ->
      forall document ^$ fun d2 ->
        d1 ^^ break ^^ d2 =~= d1 ^/^ d2

let prop_append_hardbreak () =
  check_property ^$
    forall document ^$ fun d1 ->
      forall document ^$ fun d2 ->
        d1 ^^ hardbreak ^^ d2 =~= d1 ^//^ d2

let prop_concat () =
  check_property ^$
    forall (list document) ^$ fun ds ->
      forall document ^$ fun d ->
        concat d ds =~= List.fold_left ( ^^ ) empty (intersperse d ds)

let prop_map_concat_array () =
  check_property ^$
    forall (list document) ^$ fun ds ->
      forall document ^$ fun d ->
        let f i x = (*Pretty.int i ^^*) x in
        map_concat_array f d (Array.of_list ds)
          =~=
        List.fold_left ( ^^ ) empty (intersperse d ds)

let prop_int () =
  check_property ^$
    forall (int_range (-1000) 1000) ^$ fun i ->
      text (string_of_int i) =~= Pretty.int i

let prop_bool () =
  check_property ^$
    forall bool ^$ fun b ->
      text (if b then "true" else "false") =~= Pretty.bool b

let prop_unit () =
  check_property ^$
    (text "()" =~= Pretty.unit)

let prop_string () =
  check_property ^$
    forall string ^$ fun s ->
      text ("\"" ^ String.escaped s ^ "\"") =~= Pretty.string s

let prop_char () =
  check_property ^$
    forall char ^$ fun c ->
      text ("\'" ^ Char.escaped c ^ "\'") =~= Pretty.char c

let prop_float () =
  check_property ^$
    forall (float_range (-2e30) (2e30)) ^$ fun f ->
      text (string_of_float f) =~= Pretty.float f

let prop_wrap () =
  check_property ^$
    forall document ^$ fun sep ->
      forall (list document) ^$ fun ds ->
        match ds with
          | [] -> wrap sep ds =~= empty
          | d::ds ->
            wrap sep (d::ds) =~=
              concat sep (d :: List.map (fun x -> group (break ^^ x)) ds)

(******************************************************************************)
let prop_list () =
  check_property ^$
    forall (list document) ^$ fun ds ->
      Pretty.list ds =~=
        group (align (text "["
                      ^^ alignment_spaces 1
                      ^^ concat (break_with "" ^^ text "; ") ds
                      ^^ break_with ""
                      ^^ text "]"))
let prop_tuple () =
  check_property ^$
    forall (list document) ^$ fun ds ->
      Pretty.tuple ds =~=
        align (text "("
               ^^ group (alignment_spaces 1
                         ^^ concat (break_with "" ^^ text ", ") ds
                         ^^ break_with ""
                         ^^ text ")"))

let prop_application () =
  check_property ^$
    forall document ^$ fun d ->
      forall (list document) ^$ fun ds ->
        application d ds =~=
          group (align (d ^^ nest 2 (break ^^ concat break ds)))

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

(* FIXME: and more... *)

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
      ; "nest_alignment_spaces">:: prop_nest_alignment_spaces

      ; "alignment_spaces_empty">:: prop_alignment_spaces_empty

      ; "align_empty"       >:: prop_align_empty
      ; "align_text"        >:: prop_align_text
      ; "align_group"       >:: prop_align_group
      ; "align_nest"        >:: prop_align_nest
      ; "align_align"       >:: prop_align_align

      ; "text_group"        >:: prop_text_group
      ; "group_empty"       >:: prop_group_empty
      ; "group_group"       >:: prop_group_group
      ; "group_hardbreak"   >:: prop_group_hardbreak
      ]

    ; "derived combinator properties" >:::
      [ "break_with"        >:: prop_break_with
      ; "append_space"      >:: prop_append_space
      ; "append_break"      >:: prop_append_break
      ; "append_hardbreak"  >:: prop_append_hardbreak
      ; "concat"            >:: prop_concat
      ; "map_concat_array"  >:: prop_map_concat_array
      ; "int"               >:: prop_int
      ; "bool"              >:: prop_bool
      ; "unit"              >:: prop_unit
      ; "string"            >:: prop_string
      ; "char"              >:: prop_char
      ; "float"             >:: prop_float
      ; "list"              >:: prop_list
      ; "tuple"             >:: prop_tuple
      ; "application"       >:: prop_application
      ; "wrap"              >:: prop_wrap
      ]
    ]

let _ =
  Random.self_init ();
  run_test_tt_main suite
