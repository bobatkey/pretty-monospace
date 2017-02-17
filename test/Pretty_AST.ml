open CamlCheck

type ast =
  | Empty
  | Concat of ast * ast
  | Text of string
  | Break of string
  | Nest of int * ast
  | Alignment_spaces of int
  | Group of ast
  | Align of ast

module type LAYOUT_SPEC_LANGUAGE = sig
  type t
  val empty : t
  val ( ^^ ) : t -> t -> t
  val text : string -> t
  val break_with : string -> t
  val alignment_spaces : int -> t
  val group : t -> t
  val nest : int -> t -> t
  val align : t -> t
end

module AST : LAYOUT_SPEC_LANGUAGE with type t = ast = struct
  type t = ast
  let empty = Empty
  let (^^) x y = Concat (x,y)
  let text s = Text s
  let break_with s = Break s
  let nest i x = Nest (i, x)
  let alignment_spaces i = Alignment_spaces i
  let group x = Group x
  let align x = Align x
end

let rec to_string = function
  | Empty              -> "Empty"
  | Concat (x,y)       -> "Concat (" ^ to_string x ^ "," ^ to_string y ^ ")"
  | Text s             -> "Text \"" ^ String.escaped s ^ "\""
  | Break s            -> "Break \"" ^ String.escaped s ^ "\""
  | Nest (i,x)         -> "Nest (" ^ string_of_int i ^ "," ^ to_string x ^ ")"
  | Alignment_spaces i -> "Alignment_spaces " ^ string_of_int i
  | Group x            -> "Group (" ^ to_string x ^ ")"
  | Align x            -> "Align (" ^ to_string x ^ ")"

let interp (type a) (module I : LAYOUT_SPEC_LANGUAGE with type t = a) ast =
  let open I in
  let rec interp = function
    | Empty        -> empty
    | Concat (x,y) -> interp x ^^ interp y
    | Text s       -> text s
    | Break s      -> break_with s
    | Nest (i, x)  -> nest i (interp x)
    | Alignment_spaces n -> alignment_spaces n
    | Group x            -> group (interp x)
    | Align x            -> align (interp x)
  in
  interp ast

(* Random generation of asts *)
let document_generator
    (type a) (module D : LAYOUT_SPEC_LANGUAGE with type t = a) depth =
  let open D in
  let open Generator in
  let rec make depth =
    if depth = 0 then
      int 3 >>= function
      | 0 -> return empty
      | 1 -> text <$> Domain.to_generator Domain.printable_ascii_string
      | 2 -> return (break_with " ")
      | _ -> assert false
    else
      int 5 >>= function
      | 0 -> (^^) <$> make (depth-1) <*> make (depth-1)
      | 1 -> nest <$> int 10 <*> make (depth-1)
      | 2 -> alignment_spaces <$> int 20
      | 3 -> group <$> make (depth-1)
      | 4 -> align <$> make (depth-1)
      | _ -> assert false
  in
  make depth

let rec shrink = function
  | Empty        -> [Empty]
  | Concat (x,y) -> [ x; y ] @ List.concat (List.map (fun x -> List.map (fun y -> Concat (x,y)) (shrink y)) (shrink x))
  | Text s       -> [Text s]
  | Break s      -> [Break s]
  | Alignment_spaces i -> [Alignment_spaces i]
  | Nest (i,x)   -> [x] @ List.map (fun x -> Nest (i,x)) (shrink x)
  | Group x      -> [x] @ List.map (fun x -> Group x) (shrink x)
  | Align x      -> [x] @ List.map (fun x -> Align x) (shrink x)

let shrink x =
  List.filter (fun y -> not (x = y)) (shrink x)

let ast =
  Domain.make
    ~generator:Generator.(int 10 >>= document_generator (module AST))
    ~to_string
    ~description:"document"
    ~shrink
    ()
