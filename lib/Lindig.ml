type t =
  | Empty
  | Concat of t * t
  | Text of string
  | Nest of int * t
  | Break of string
  | Group of t
  | Align of t
  | Alignment_spaces of int
  | Hardbreak

let empty = Empty
let (^^) x y = Concat (x,y)
let text s = Text s
let break_with s = Break s
let alignment_spaces i = if i < 0 then invalid_arg "Pretty.alignment_spaces" else Alignment_spaces i
let group x = Group x
let nest i x = if i < 0 then invalid_arg "Pretty.nest" else Nest (i,x)
let align x = Align x

type mode = F | B

(* [fits remaining items] simulates the printing of [items] to check
   whether the first breaking linebreak in [items] occurs within
   [remaining] output spaces. *)
let rec fits remaining = function
  | _ when remaining < 0            -> false
  | []                              -> true
  | (i,m,Empty)::z                  -> fits remaining z
  | (i,m,Concat (x,y))::z           -> fits remaining ((i,m,x)::(i,m,y)::z)
  | (i,m,Text s)::z                 -> fits (remaining - String.length s) z
  | (i,m,(Nest (_,x) | Align x))::z -> fits remaining ((i,m,x)::z)
  | (i,F,Break s)::z                -> fits (remaining - String.length s) z
  | (i,B,Break s)::z                -> true
  | (i,F,Hardbreak)::z              -> false
  | (i,B,Hardbreak)::z              -> true
  | (i,m,Group x)::z                -> fits remaining ((i,m,x)::z)
  | (i,F,Alignment_spaces j)::z     -> fits remaining z
  | (i,B,Alignment_spaces j)::z     -> fits (remaining(*-j*)) z

let render w d =
  let b = Buffer.create 32 in
  let emit s = Buffer.add_string b s
  and spaces i = for _ = 1 to i do Buffer.add_char b ' ' done
  and nl () = Buffer.add_char b '\n'
  in
  let rec format c = function
    | []                          -> ()
    | (i,m,Empty)::z              -> format c z
    | (i,m,Concat (x,y))::z       -> format c ((i,m,x)::(i,m,y)::z)
    | (i,m,Text s)::z             -> emit s; format (c+String.length s) z
    | (i,m,Nest (j,x))::z         -> format c ((i+j,m,x)::z)
    | (i,m,Align x)::z            -> format c ((c,m,x)::z)
    | (i,F,Break s)::z            -> emit s; format (c+String.length s) z
    | (i,B,Break _)::z            -> nl (); spaces i; format i z
    | (i,B,Hardbreak)::z          -> nl (); spaces i; format i z
    | (i,F,Hardbreak)::z          -> assert false
    | (i,B,Alignment_spaces j)::z -> spaces j; format (c+j) z
    | (i,F,Alignment_spaces _)::z -> format c z
    | (i,F,Group x)::z            -> format c ((i,F,x)::z)
    | (i,B,Group x)::z            ->
       if fits (w-c) ((i,F,x)::z) then
         format c ((i,F,x)::z)
       else
         format c ((i,B,x)::z)
  in
  format 0 [(0,B,d)];
  Buffer.contents b
