(* layout strategy:
   - a group is laid out flat if it will fit on the current line, including the trailing jobs *)

(* a group is laid out flat if there is an upcoming break that will
   make this whole line fit. Otherwise, use line breaks. Would like to
   avoid re-measuring groups if possible. *)

(* Want to annotate the [Start_group]s in the input stream with:
   (a) the flat width of every group
   (b) the distance from the end of each group to the nearest break opportunity

   with this information, we can then decide how to lay out each group
   as we meet it in the stream. A naive way to gather this information
   requires the entire stream:

     - a group may cover the entire stream

     - the next break might be at the end of the input

   However, these are both bounded by the line width, so we only need
   some kind of sliding window that looks ahead until the end of the
   current line.

   processing an annotated stream:
   - Text:       output it
   - StartGroup: if breaking, and it fits, then switch to flat mode for this group
   - EndGroup:   pop the most recent break decision
   - Break:      if breaking, then newline; else space
*)

module Pretty : sig
  type t

  val init        : int -> t
  val text        : t -> string -> unit
  val start_group : t -> unit
  val end_group   : t -> unit
  val break       : t -> unit
  val start_nest  : t -> int -> unit
  val end_nest    : t -> unit
  val finish      : t -> unit
end = struct

  type group_status =
    | TooFar
    | Fixed of int
    | Measuring of int

  type event =
    | Text of string

    | Start_group of group_status ref
    | End_group

    | Break
    | Start_nest of int
    | End_nest
    | Start_align
    | End_align

  type t =
    { queue            : event Queue.t

    (* configuration *)
    ; width            : int

    (* measurement *)
    ; open_groups      : group_status ref CCDeque.t
    (* FIXME: could get rid of this second queue if we dynamically
       extended the extent of all groups to cover all trailing text (see
       Chitil's paper) *)
    ; closed_groups    : group_status ref Queue.t

    (* layout variables *)
    ; mutable flat     : int (* number of 'start_group's we are nested in, in flat mode *)
    ; mutable column   : int
    ; mutable indent   : int
    ;         indents  : int Stack.t
    }

  let init width =
    { queue         = Queue.create ()
    ; open_groups   = CCDeque.create ()
    ; closed_groups = Queue.create ()
    ; width
    ; flat          = 0
    ; column        = 0
    ; indent        = 0
    ; indents       = Stack.create ()
    }

  let layout st =
    (* whenever we get stuck in layout, the head of the queue will
       always be a Start_group, and the start_groups will always be in
       the order they are in the open_ and closed_groups queues.

       This means that we know how far to step through the queue by
       counting the number of start_groups that have been evicted in
       the last overflow check, or now have fixed measures instead of
       dynamically checking them. *)
    let is_live = function
      | Text _ | Break | End_group
      | Start_nest _ | End_nest | Start_align | End_align
      | Start_group {contents=TooFar | Fixed _} -> true
      | Start_group {contents=Measuring _} -> false
    in
    while not (Queue.is_empty st.queue) && is_live (Queue.peek st.queue) do
      match Queue.take st.queue with
        | Text s ->
           print_string s;
           st.column <- st.column + String.length s
        | Start_group {contents=Measuring _} ->
           assert false
        | Start_group {contents=TooFar} ->
           assert (st.flat = 0)
        | Start_group {contents=Fixed w} when st.flat > 0 ->
           st.flat <- st.flat + 1
        | Start_group {contents=Fixed w} when st.column + w <= st.width ->
           st.flat <- 1
        | Start_group {contents=Fixed w} ->
           ()
        | End_group ->
           if st.flat > 0 then st.flat <- st.flat - 1
        | Break ->
           if st.flat = 0 then
             (print_newline ();
              print_string (String.make st.indent ' ');
              st.column <- st.indent)
           else
             print_string " "
        | Start_nest i ->
           Stack.push st.indent st.indents;
           st.indent <- st.indent + i
        | Start_align ->
           Stack.push st.indent st.indents;
           st.indent <- st.column
        | End_nest | End_align ->
           (match Stack.pop st.indents with
             | exception Stack.Empty -> failwith "badly nested nests"
             | i -> st.indent <- i)
    done

  let evict_overflowed_groups st =
    let overflowing r = match !r with
      | TooFar | Fixed _ -> assert false
      | Measuring i -> i > st.width
    in
    (* We rely on the invariant that
       - everything in the measuring queues is [Measuring]
       - the elements are in ascending order
    *)
    while not (Queue.is_empty st.closed_groups)
          && overflowing (Queue.peek st.closed_groups)
    do
      Queue.take st.closed_groups := TooFar
    done;
    while not (CCDeque.is_empty st.open_groups)
          && overflowing (CCDeque.peek_front st.open_groups)
    do
      CCDeque.take_front st.open_groups := TooFar
    done
  (* invariant: if we evict any open_groups, then we must have already
     evicted all the closed_groups. *)
  
  (* We could just keep a count of how many open groups we need to
     process now, and then run layout to process them. *)

  let increment_by i r = match !r with
    | TooFar | Fixed _ -> assert false
    | Measuring j      -> r := Measuring (i+j)
    
  let text st txt =
    if String.length txt > 0 then begin
      (* FIXME: if the item at the head of the queue is a start_group,
         then switch the order. This reduces the latency of the
         printer so that text is always output immediately if we
         aren't waiting for a decision on a line break. *)
      Queue.push (Text txt) st.queue;
      let w = String.length txt in
      (* is there a way of doing this that isn't linear in the nesting
         depth/dynamic extent of groups? *)
      CCDeque.iter (increment_by w) st.open_groups;
      Queue.iter (increment_by w) st.closed_groups;
      evict_overflowed_groups st;
      layout st
    end

  let start_group st =
    let measure = ref (Measuring 0) in
    Queue.push (Start_group measure) st.queue;
    CCDeque.push_back st.open_groups measure

  let end_group st =
    (* FIXME: could track group nesting here to track errors; or track
       it later on in the layout phase? *)
    Queue.push End_group st.queue;
    if not (CCDeque.is_empty st.open_groups) then
      let measurer = CCDeque.take_back st.open_groups in
      Queue.push measurer st.closed_groups

  let fix_measure r = match !r with
    | TooFar | Fixed _ -> assert false
    | Measuring i -> r := Fixed i

  let break st =
    Queue.push Break st.queue;
    Queue.iter fix_measure st.closed_groups;
    Queue.clear st.closed_groups;
    CCDeque.iter (increment_by 1) st.open_groups;
    evict_overflowed_groups st;
    layout st

  let finish st =
    assert (CCDeque.is_empty st.open_groups);
    Queue.iter fix_measure st.closed_groups;
    Queue.clear st.closed_groups;
    layout st;
    assert (Queue.is_empty st.queue)

  let start_nest st i =
    (* FIXME: keep track of the nesting of 'nest's, 'align's and
       'group's to spot errors. *)
    Queue.push (Start_nest i) st.queue

  let end_nest st =
    Queue.push End_nest st.queue

  let start_align st =
    Queue.push Start_align st.queue

  let end_align =
    Queue.push End_align
end

module Document = struct
  type t =
    | Emp
    | Concat of t * t
    | Text of string
    | Break
    | Group of t
    | Nest of int * t

  let render width doc =
    let pp = Pretty.init width in
    let rec render = function
      | Emp -> ()
      | Concat (x, y) -> render x; render y
      | Text s        -> Pretty.text pp s
      | Break         -> Pretty.break pp
      | Group x ->
         Pretty.start_group pp;
         render x;
         Pretty.end_group pp
      | Nest (i, x) ->
         Pretty.start_nest pp i;
         render x;
         Pretty.end_nest pp
    in
    render doc;
    Pretty.finish pp

  let empty = Emp
  let (^^) x y = Concat (x, y)
  let text s = Text s
  let break = Break
  let group x = Group x
  let nest i x = Nest (i,x)
  let (^/^) x y = x ^^ break ^^ y
end

let test_lineleft_doc =
  Document.(group
    (text "begin"
     ^^ nest 3 (break
                ^^ group (text "stmt;"
                          ^/^ text "stmt;"
                          ^/^ text "stmt;"))
     ^^ text "end"))

let test_lineleft_doc2 =
  Document.(group
    (text "begin"
     ^^ nest 3 (break
                ^^ group (text "stmt;"
                          ^/^ text "stmt;"
                          ^/^ text "stmt;"))
     ^/^ text "end"))

let group f pp =
  Pretty.start_group pp;
  f pp;
  Pretty.end_group pp

let nest i f pp =
  Pretty.start_nest pp i;
  f pp;
  Pretty.end_nest pp
  
let text s pp =
  Pretty.text pp s

let (^^) f g pp =
  f pp;
  g pp

let test w =
  Pretty.init w |>
  text "["
  ^^
  nest 1 (group
    begin
      text "1"
      ^^
      Pretty.break
      ^^
      text "2"
      ^^
      Pretty.break
      ^^
      text "3"
      ^^
      Pretty.break
      ^^
      text "4"
    end)
  ^^
  text "]"
  ^^
  Pretty.finish
