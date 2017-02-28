(* (C) Robert Atkey 2013-2014, see LICENSE for more information. *)

open OUnit
open CamlCheck

module type PRETTY_DOC = sig
  include Pretty_AST.LAYOUT_SPEC_LANGUAGE

  val render : int -> t -> string
end

module Doc_utils (D : PRETTY_DOC) = struct
  include D

  let break = break_with " "
  let (^/^) x y = x ^^ break ^^ y
end

module Make_comparison_test (D1 : PRETTY_DOC) (D2 : PRETTY_DOC) = struct
  module D1 = Doc_utils (D1)
  module D2 = Doc_utils (D2)

  let equal_render_test =
    "equal_render" >: Property.to_test
      (Property.forall Pretty_AST.ast @@ fun d ->
       Property.forall (Domain.int_range 0 200) @@ fun width ->
       Property.equal ~to_string:(fun s -> "\"" ^ String.escaped s ^ "\"")
         (D1.render width (Pretty_AST.interp (module D1) d))
         (D2.render width (Pretty_AST.interp (module D2) d)))
end

module Contextual_Equivalence_Tests (D : PRETTY_DOC) = struct
  open D
  include Doc_utils (D)

  let document_generator =
    Pretty_AST.document_generator (module D)
  
  let document =
    Domain.make
      ~generator:Generator.(int 10 >>= document_generator)
      ~to_string:(fun _ -> "??")
      ~description:"document"
      ()

  (******************************************************************************)
  type document_context =
    | Hole
    | Concat1 of document_context * D.t
    | Concat2 of D.t * document_context
    | Nest    of int * document_context
    | Group   of document_context
    | Align   of document_context

  let rec string_of_document_context = function
    | Hole         -> "Hole"
    | Concat1(c,d) -> "Concat1(" ^ string_of_document_context c ^ ", " ^ "??"(*to_string d*) ^ ")"
    | Concat2(d,c) -> "Concat2(" ^ "??"(*to_string d*) ^ ", " ^ string_of_document_context c ^ ")"
    | Nest(n,c)    -> "Nest(" ^ string_of_int n ^ ", " ^ string_of_document_context c ^ ")"
    | Group c      -> "Group(" ^ string_of_document_context c ^ ")"
    | Align c      -> "Align(" ^ string_of_document_context c ^ ")"

  let (@//) context document =
    let rec plug = function
      | Hole                   -> document
      | Concat1 (context, doc) -> plug context ^^ doc
      | Concat2 (doc, context) -> doc ^^ plug context
      | Group   context        -> group (plug context)
      | Nest    (i,context)    -> nest i (plug context)
      | Align   context        -> align (plug context)
    in
    plug context

  let append1 x y = Concat1 (x,y)
  let append2 x y = Concat2 (x,y)
  let nest_ctxt n x = Nest (n,x)
  let group_ctxt x = Group x
  let align_ctxt x = Align x

  let document_context =
    let open Generator in
    let rec make depth =
      if depth = 0 then return Hole
      else
        int 5 >>= function
        | 0 -> append1 <$> make (depth-1) <*> document_generator (depth-1)
        | 1 -> append2 <$> document_generator (depth-1) <*> make (depth-1)
        | 2 -> nest_ctxt <$> int 10 <*> make (depth-1)
        | 3 -> group_ctxt <$> make (depth-1)
        | 4 -> align_ctxt <$> make (depth-1)
        | _ -> assert false
    in
    let rec shrink = function
      | Hole -> []
      | Concat1 (c,d) -> c :: List.map (fun c' -> Concat1 (c',d)) (shrink c)
      | Concat2 (d,c) -> c :: List.map (fun c' -> Concat2 (d,c')) (shrink c)
      | Nest (n,c) -> c :: if n > 0 then [Nest (n-1,c)] else []
      | Group c -> c :: List.map (fun c' -> Group c') (shrink c)
      | Align c -> c :: List.map (fun c' -> Align c') (shrink c)
    in
    Domain.make
      ~generator:(int 10 >>= make)
      ~to_string:string_of_document_context
      ~description:"document_context"
      ~shrink
      ()

  (** Property that states that two documents are contextually
      equivalent. *)
  let ( =~= ) document1 document2 =
    Property.forall (Domain.int_range 0 200) @@ fun width ->
    Property.forall document_context @@ fun c ->
    Property.equal ~to_string:(Printf.sprintf "%S")
      (render width (c @// document1))
      (render width (c @// document2))
end

module Property_tests (D : PRETTY_DOC) : sig
  val suite : OUnit.test list
end = struct
  open D

  include Contextual_Equivalence_Tests (D)

  (* Equational properties of the combinators *)
  let prop_empty_left_unit =
    Property.forall document @@ fun d ->
    d =~= empty ^^ d

  let prop_empty_right_unit =
    Property.forall document @@ fun d ->
    d =~= d ^^ empty

  let prop_append_assoc =
    Property.forall document @@ fun d1 ->
    Property.forall document @@ fun d2 ->
    Property.forall document @@ fun d3 ->
    d1 ^^ (d2 ^^ d3) =~= (d1 ^^ d2) ^^ d3

  let prop_text_append =
    Property.forall Domain.printable_ascii_string @@ fun s1 ->
    Property.forall Domain.printable_ascii_string @@ fun s2 ->
    text s1 ^^ text s2 =~= text (s1 ^ s2)

  let prop_text_empty =
    text "" =~= empty

  let prop_text_group =
    Property.forall document @@ fun d ->
    Property.forall Domain.printable_ascii_string @@ fun s ->
    group (text s ^^ d) =~= text s ^^ group d

  let prop_group_text =
    Property.forall document @@ fun d ->
    Property.forall Domain.printable_ascii_string @@ fun s ->
    group (d ^^ text s) =~= group d ^^ text s

  let prop_nest_group =
    Property.forall document @@ fun d ->
    Property.forall (Domain.int_range 0 12) @@ fun n ->
    group (nest n d) =~= nest n (group d)

  let prop_align_group =
    Property.forall document @@ fun d ->
    group (align d) =~= align (group d)

  let prop_zero_nest =
    Property.forall document @@ fun d ->
    nest 0 d =~= d

  let prop_nest_nest =
    Property.forall (Domain.int_range 0 24) @@ fun n1 ->
    Property.forall (Domain.int_range 0 24) @@ fun n2 ->
    Property.forall document @@ fun d ->
    nest n1 (nest n2 d) =~= nest (n1 + n2) d

  let prop_nest_text =
    Property.forall document @@ fun d ->
    Property.forall Domain.printable_ascii_string @@ fun s ->
    Property.forall (Domain.int_range 0 24) @@ fun n ->
    text s ^^ nest n d =~= nest n (text s ^^ d)

  let prop_text_nest =
    Property.forall document @@ fun d ->
    Property.forall Domain.printable_ascii_string @@ fun s ->
    Property.forall (Domain.int_range 0 24) @@ fun n ->
    nest n d ^^ text s =~= nest n (d ^^ text s)

  let prop_nest_alignment_spaces =
    Property.forall document @@ fun d ->
    Property.forall (Domain.int_range 0 40) @@ fun n1 ->
    Property.forall (Domain.int_range 0 40) @@ fun n2 ->
    alignment_spaces n1 ^^ nest n2 d =~= nest n2 (alignment_spaces n1 ^^ d)

  let prop_alignment_spaces_nest =
    Property.forall document @@ fun d ->
    Property.forall (Domain.int_range 0 40) @@ fun n1 ->
    Property.forall (Domain.int_range 0 40) @@ fun n2 ->
    nest n2 d ^^ alignment_spaces n1 =~= nest n2 (d ^^ alignment_spaces n1)

  let prop_alignment_spaces_empty =
    alignment_spaces 0 =~= empty

  let prop_alignment_spaces_add =
    Property.forall (Domain.int_range 0 40) @@ fun n1 ->
    Property.forall (Domain.int_range 0 40) @@ fun n2 ->
    alignment_spaces n1 ^^ alignment_spaces n2 =~= alignment_spaces (n1 + n2)

  let prop_align_text =
    Property.forall document @@ fun d ->
    Property.forall Domain.printable_ascii_string @@ fun s ->
    align (text s ^^ nest (String.length s) d) =~= text s ^^ align d

  let prop_align_nest =
    Property.forall document @@ fun d ->
    Property.forall (Domain.int_range 1 12) @@ fun n ->
    nest n (align d) =~= align d

  let prop_align_align =
    Property.forall document @@ fun d ->
    align (align d) =~= align d

  let prop_group_empty =
    group empty =~= empty

  let prop_nest_empty =
    Property.forall (Domain.int_range 0 12) @@ fun n ->
    nest n empty =~= empty

  let prop_align_empty =
    align empty =~= empty

  let prop_group_group =
    Property.forall document @@ fun d ->
    group (group d) =~= group d

  (******************************************************************************)
  (* Properties of the combinators on bad input *)

  let prop_exn_nest =
    Property.forall document @@ fun d ->
    Property.forall (Domain.int_range (-2000) (-1)) @@ fun n ->
    Property.raises
      (Invalid_argument "Pretty.nest")
      (fun () -> nest n d)

  let prop_exn_alignment_spaces =
    Property.forall (Domain.int_range (-2000) (-1)) @@ fun n ->
    Property.raises
      (Invalid_argument "Pretty.alignment_spaces")
      (fun () -> alignment_spaces n)

  (******************************************************************************)
  (* Rendering tests *)

  let check_render width ~document ~expected_output =
    assert_equal ~printer:(Printf.sprintf "%S")
      expected_output
      (render width document)

  let test_text () =
    check_render
      80
      ~document:(text "xxx")
      ~expected_output:"xxx"

  let test_append () =
    check_render
      80
      ~document:(text "xxx" ^^ text "yyy")
      ~expected_output:"xxxyyy"

  let test_empty () =
    check_render
      80
      ~document:empty
      ~expected_output:""

  let test_break1 () =
    check_render
      80
      ~document:(group (text "xxx" ^^ break ^^ text "yyy"))
      ~expected_output:"xxx yyy"

  let test_break2 () =
    check_render
      2
      ~document:(text "xxx" ^^ break ^^ text "yyy")
      ~expected_output:"xxx\nyyy"

  let test_nest1 () =
    check_render
      3
      ~document:(group (text "xxx" ^^ nest 2 (break ^^ text "yyy")))
      ~expected_output:"xxx\n  yyy"

  let test_nest2 () =
    check_render
      10
      ~document:(group (text "xxx" ^^ nest 2 (break ^^ text "yyy")))
      ~expected_output:"xxx yyy"

  let test_nest3 () =
    check_render
      10
      ~document:(text "xxx" ^^ nest 3 (group (break ^^ text "yyy")) ^/^ text "zzzz")
      ~expected_output:"xxx yyy\nzzzz"

  let test_nest4 () =
    check_render
      10
      ~document:(text "xxx" ^^ nest 3 (group (break ^^ text "yyy") ^/^ text "zzzz"))
      ~expected_output:"xxx yyy\n   zzzz"

  let test_align1 () =
    check_render
      9
      ~document:(text "xxx" ^^ align (text "yyy" ^^ break ^^ text "zzz"))
      ~expected_output:"xxxyyy\n   zzz"

  let test_align2 () =
    check_render
      10
      ~document:(group (text "xxx" ^^ align (text "yyy" ^^ break ^^ text "zzz")))
      ~expected_output:"xxxyyy zzz"

  let test_group1 () =
    check_render
      10
      ~document:(group (text "xxx" ^/^ text "yyy") ^/^ text "zzzzzzzzzz")
      ~expected_output:"xxx yyy\nzzzzzzzzzz"

  let test_group2 () =
    check_render
      10
      ~document:(text "xxx"
                 ^^ group (break ^^ group (text "yyy" ^^ break ^^ text "zzzz")))
      ~expected_output:"xxx\nyyy zzzz"

  let test_break_with1 () =
    check_render
      10
      ~document:(group (text "xxx" ^^ break_with "---" ^^ text "yyy"))
      ~expected_output:"xxx---yyy"

  let test_break_with2 () =
    check_render
      8
      ~document:(text "xxx" ^^ break_with "---" ^^ text "yyy")
      ~expected_output:"xxx\nyyy"

  let test_alignment_spaces1 () =
    check_render
      7
      ~document:(text "xxx" ^^ alignment_spaces 3 ^^ text "=" ^^ break ^^ text "yyy")
      ~expected_output:"xxx   =\nyyy"

  let test_alignment_spaces2 () =
    check_render
      8
      ~document:(group (text "xxx" ^^ alignment_spaces 3 ^^ text "=" ^^ break ^^ text "yyy"))
      ~expected_output:"xxx= yyy"

  let test_alignment_spaces3 () =
    (* Check that alignment_spaces do not count towards the
       measurement of how much space is left on a line *)
    check_render
      6
      ~document:(group break ^^ alignment_spaces 8)
      ~expected_output:"         "

  (* These three tests test that the breaking computation takes into
     account all of the queued print jobs on the current line *)
  let test_lineleft_doc =
    group
      (text "begin"
       ^^ nest 3 (break
                  ^^ group (text "stmt;"
                            ^/^ text "stmt;"
                            ^/^ text "stmt;"))
       ^^ text "end")

  let test_lineleft1 () =
    check_render
      10
      ~document:test_lineleft_doc
      ~expected_output:"begin\n   stmt;\n   stmt;\n   stmt;end"

  let test_lineleft2 () =
    check_render
      20
      ~document:test_lineleft_doc
      ~expected_output:"begin\n   stmt;\n   stmt;\n   stmt;end"

  let test_lineleft3 () =
    check_render
      30
      ~document:test_lineleft_doc
      ~expected_output:"begin stmt; stmt; stmt;end"

(*
(******************************************************************************)
(* Tests to check that the output functions agree with each other *)
let prop_custom_output =
  Property.forall document @@ fun d ->
  Property.forall (Domain.int_range 1 100) @@ fun n ->
  let b = Buffer.create 8192 in
  Pretty.custom
    ~width:n
    ~output_text:(Buffer.add_string b)
    ~output_newline:(fun () -> Buffer.add_char b '\n')
    ~output_spaces:(fun n -> Buffer.add_string b (String.make n ' '))
    d;
  Property.equal
    ~to_string:String.escaped
    (Buffer.contents b)
    (Pretty.render n d)
*)

  (******************************************************************************)
  let suite =
    [ "rendering tests" >:::
      [ "text"            >:: test_text
      ; "append"          >:: test_append
      ; "empty"           >:: test_empty
      ; "break1"          >:: test_break1
      ; "break2"          >:: test_break2
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
      ; "alignment_spaces3">:: test_alignment_spaces3
      ; "lineleft1"       >:: test_lineleft1
      ; "lineleft2"       >:: test_lineleft2
      ; "lineleft3"       >:: test_lineleft3
      ]

    ; "exception tests" >:::
      [ "nest"             >: Property.to_test prop_exn_nest
      ; "alignment_spaces" >: Property.to_test prop_exn_alignment_spaces
      ]

    ; "combinator properties" >:::
      [ "empty_left_unit"   >: Property.to_test prop_empty_left_unit
      ; "empty_right_unit"  >: Property.to_test prop_empty_right_unit
      ; "append_assoc"      >: Property.to_test prop_append_assoc

      ; "text_append"       >: Property.to_test prop_text_append
      ; "text_empty"        >: Property.to_test prop_text_empty

      ; "nest_text"         >: Property.to_test prop_nest_text
      ; "text_nest"         >: Property.to_test prop_text_nest
      ; "nest_empty"        >: Property.to_test prop_nest_empty
      ; "nest_group"        >: Property.to_test prop_nest_group
      ; "zero_nest"         >: Property.to_test prop_zero_nest
      ; "nest_nest"         >: Property.to_test prop_nest_nest
      ; "nest_alignment_spaces">: Property.to_test prop_nest_alignment_spaces
      ; "alignment_spaces_nest">: Property.to_test prop_alignment_spaces_nest

      ; "alignment_spaces_empty">: Property.to_test prop_alignment_spaces_empty
      ; "alignment_spaces_add" >: Property.to_test prop_alignment_spaces_add

      ; "align_empty"       >: Property.to_test prop_align_empty
      ; "align_text"        >: Property.to_test prop_align_text
      ; "align_group"       >: Property.to_test prop_align_group
      ; "align_nest"        >: Property.to_test prop_align_nest
      ; "align_align"       >: Property.to_test prop_align_align

      ; "text_group"        >: Property.to_test prop_text_group
      ; "group_text"        >: Property.to_test prop_group_text
      ; "group_empty"       >: Property.to_test prop_group_empty
      ; "group_group"       >: Property.to_test prop_group_group
      ]
    ]
end

module type PRETTY_DOC_DERIVED = sig
  include PRETTY_DOC

  val break : t
  val spaces : int -> t
  val space : t
  val ( ^+^ ) : t -> t -> t
  val ( ^/^ ) : t -> t -> t
  val join : t -> t list -> t
  val map_join : ('a -> t) -> t -> 'a list -> t
  val join_array : t -> t array -> t
  val map_join_array : (int -> 'a -> t) -> t -> 'a array -> t
  val wrap : t -> t list -> t
  val wrap_array : t -> t array -> t
end

module Derived_Combinator_Tests (D : PRETTY_DOC_DERIVED) = struct
  open D
  include Contextual_Equivalence_Tests (D)

  let intersperse separator xs =
    let rec loop acc = function
      | []    -> List.rev acc
      | x::xs ->
         match acc with
           | [] -> loop [x] xs
           | _  -> loop (x::separator::acc) xs
    in loop [] xs

  let prop_break_with =
    break =~= break_with " "

  let prop_spaces =
    Property.forall (Domain.int_range 0 20) @@ fun n ->
    spaces n =~= text (String.make n ' ')

  let prop_space =
    space =~= text " "

  let prop_append_space =
    Property.forall document @@ fun d1 ->
    Property.forall document @@ fun d2 ->
    d1 ^^ text " " ^^ d2 =~= d1 ^+^ d2

  let prop_append_break =
    Property.forall document @@ fun d1 ->
    Property.forall document @@ fun d2 ->
    d1 ^^ break ^^ d2 =~= d1 ^/^ d2

  let prop_join =
    Property.forall (Domain.list document) @@ fun ds ->
    Property.forall document @@ fun d ->
    join d ds =~= List.fold_left ( ^^ ) empty (intersperse d ds)

  let prop_map_join =
    Property.forall (Domain.list document) @@ fun ds ->
    Property.forall document @@ fun d ->
    Property.forall document @@ fun d' ->
    let f x = x ^^ d' in
    map_join f d ds =~=
    List.fold_left ( ^^ ) empty (intersperse d (List.map f ds))

  let prop_join_array =
    Property.forall (Domain.list document) @@ fun ds ->
    Property.forall document @@ fun d ->
    join_array d (Array.of_list ds)
    =~=
    List.fold_left ( ^^ ) empty (intersperse d ds)

  let prop_map_join_array =
    Property.forall (Domain.list document) @@ fun ds ->
    Property.forall document @@ fun d ->
    map_join_array (fun _ x -> x) d (Array.of_list ds)
    =~=
    List.fold_left ( ^^ ) empty (intersperse d ds)

  let prop_wrap =
    Property.forall document @@ fun sep ->
    Property.forall (Domain.list document) @@ fun ds ->
    match ds with
      | [] -> wrap sep ds =~= empty
      | d::ds ->
         wrap sep (d::ds) =~=
         join sep (d :: List.map (fun x -> group (break ^^ x)) ds)

  let prop_wrap_array =
    Property.forall document @@ fun sep ->
    Property.forall (Domain.list document) @@ fun ds ->
    wrap sep ds =~= wrap_array sep (Array.of_list ds)

  let prop_exn_spaces =
    Property.forall (Domain.int_range (-100) (-1)) @@ fun n ->
    Property.raises
      (Invalid_argument "Pretty.spaces")
      (fun () -> spaces n)

  let suite =
    [ "break_with"        >: Property.to_test prop_break_with
    ; "spaces"            >: Property.to_test prop_spaces
    ; "space"             >: Property.to_test prop_space
    ; "append_space"      >: Property.to_test prop_append_space
    ; "append_break"      >: Property.to_test prop_append_break
    ; "join"              >: Property.to_test prop_join
    ; "map_join"          >: Property.to_test prop_map_join
    ; "join_array"        >: Property.to_test prop_join_array
    ; "map_join_array"    >: Property.to_test prop_map_join_array
    ; "wrap"              >: Property.to_test prop_wrap
    ; "wrap_array"        >: Property.to_test prop_wrap_array
    ; "spaces"            >: Property.to_test prop_exn_spaces
    ]
end

module Eager = struct
  include Pretty.Doc
  let render width doc =
    let b      = Buffer.create 128 in
    let output = Pretty.Output.to_buffer b in
    Pretty.Doc.format output ~width doc;
    Buffer.contents b
end

module Streaming = struct
  type t =
    | Emp
    | Concat of t * t
    | Text of string
    | Break of string
    | Alignment_spaces of int
    | Group of t
    | Nest of int * t
    | Align of t

  open Pretty.Stream

  let render width doc =
    let b = Buffer.create 128 in
    let output = Pretty.Output.to_buffer b in
    let pp = create output ~width in
    let rec render = function
      | Emp -> ()
      | Concat (x, y) -> render x; render y
      | Text s        -> text pp s
      | Break s       -> break pp s
      | Alignment_spaces i -> alignment_spaces pp i
      | Group x ->
         start_group pp;
         render x;
         end_group pp
      | Nest (i, x) ->
         start_nest pp i;
         render x;
         end_nest_or_align pp
      | Align x ->
         start_align pp;
         render x;
         end_nest_or_align pp
    in
    render doc;
    flush pp;
    Buffer.contents b

  let empty = Emp
  let (^^) x y = Concat (x, y)
  let text s = Text s
  let break_with s = Break s
  let group x = Group x
  let nest i x =
    if i < 0 then invalid_arg ("Pretty.nest")
    else Nest (i,x)
  let alignment_spaces i =
    if i < 0 then invalid_arg ("Pretty.alignment_spaces")
    else Alignment_spaces i
  let align x = Align x

end

module Eager_Props        = Property_tests (Eager)
module Eager_Derived      = Derived_Combinator_Tests (Eager)
module Tests_of_Streaming = Property_tests (Streaming)
module Lindig_Props       = Property_tests (Lindig)
module Comparison         = Make_comparison_test (Eager) (Streaming)
module Comparison2        = Make_comparison_test (Lindig) (Streaming)

let _ =
  run_test_tt_main
    ("all tests" >:::
     [ "eager"         >::: Eager_Props.suite
     ; "streaming"     >::: Tests_of_Streaming.suite
     ; "lindig"        >::: Lindig_Props.suite
     ; "comparison"    >:   Comparison.equal_render_test
     ; "comparison2"   >:   Comparison2.equal_render_test
     ; "eager derived" >::: Eager_Derived.suite
     ])
