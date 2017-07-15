(* (C) Robert Atkey 2013-2017, see LICENSE for more information. *)

module Output = struct
  type t =
    { text    : string -> unit
    ; char    : char -> unit
    ; newline : unit -> unit
    ; spaces  : int -> unit
    }

  let n_spaces = 20

  let ready_made_spaces = String.make n_spaces ' '

  let to_channel ch =
    let text s = output_string ch s in
    let char c = output_char ch c in
    let newline () = output_char ch '\n' in
    let rec spaces i =
      if i = 0 then ()
      else if i < n_spaces then
        output_substring ch ready_made_spaces 0 i
      else
        (output_string ch ready_made_spaces;
         spaces (i - n_spaces))
    in
    { text; char; newline; spaces }

  let stdout = to_channel stdout
  let stderr = to_channel stderr

  let to_buffer buf =
    let text s = Buffer.add_string buf s in
    let char c = Buffer.add_char buf c in
    let newline () = Buffer.add_char buf '\n' in
    let rec spaces i =
      if i = 0 then ()
      else if i < n_spaces then
        Buffer.add_substring buf ready_made_spaces 0 i
      else
        (Buffer.add_string buf ready_made_spaces;
         spaces (i - n_spaces))
    in
    { text; char; newline; spaces }

  let custom ~text ~char ~newline ~spaces =
    { text; char; newline; spaces }
end

module Doc = struct
  type document =
    { node       : document_node
    ; flat_width : int
    (** Pre-computed width of this document were it to be formatted
        without line breaks. *)
    ; break_dist : int
    ; break_type : [`Break|`NoBreak]
    (** Internal distance to either a line break opportunity
       ([`Break]), or to the end of the document if there is no nested
       break opportunity ([`NoBreak]). *)
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

  let (+/) {break_type;break_dist} bd = match break_type with
    | `Break   -> break_dist
    | `NoBreak -> break_dist + bd

  type t = document

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
      ; break_dist = 0
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

  let fold ~empty ~concat ~text ~break ~alignspaces ~nest ~align ~group doc =
    (* FIXME: tail recursive using an explicit stack? *)
    let rec fold break_distance = function
      | { node = Empty } ->
         empty
      | { node = Concat (d1, d2) } ->
         concat (fold (d2 +/ break_distance) d1) (fold break_distance d2)
      | { node = Text s } ->
         text s
      | { node = Break s } ->
         break s
      | { node = AlignSpaces n } ->
         alignspaces n
      | { node = Nest (i, d) } ->
         nest i (fold break_distance d)
      | { node = Align d } ->
         align (fold break_distance d)
      | { node = Group d; flat_width } ->
         group (flat_width + break_distance) (fold break_distance d)
    in
    fold 0 doc

  let format out ?(width=80) doc =
    let open Output in
    let rec flat = function
      | {node=Empty | AlignSpaces _}          -> ()
      | {node=Concat (x,y)}                   -> flat x; flat y
      | {node=Text s | Break s}               -> out.text s
      | {node=Align x | Nest (_,x) | Group x} -> flat x
    in
    let rec process column bd i = function
      | {node=Empty} -> column
      | {node=Concat (x,y)} ->
         let column = process column (y +/ bd) i x in
         process column bd i y
      | {node=Text s} ->
         out.text s; column + String.length s
      | {node=AlignSpaces n} ->
         out.spaces n; column + n
      | {node=Align x} ->
         process column bd column x
      | {node=Nest (j, x)} ->
         process column bd (i+j) x
      | {node=Break _} ->
         out.newline (); out.spaces i; i
      | {node=Group x; flat_width} ->
         if width - column - flat_width >= bd then
           (flat x;
            column + flat_width)
         else
           process column bd i x
    in
    ignore (process 0 0 0 doc)

  let print ?width doc =
    format Output.stdout ?width doc

(*
  let render text newline spaces width doc =
    ignore @@ fold
      ~empty:(fun b i col -> col)
      ~concat:(fun x y b i col -> y b i (x b i col))
      ~text:(fun s b i col -> text s; col + String.length s)
      ~break:(fun s b i col ->
          if b then (newline (); spaces i; i)
          else (text s; col+String.length s))
      ~alignspaces:(fun n b i col ->
          if b then col else (spaces n; col + n))
      ~nest:(fun j d b i col -> d b (i+j) col)
      ~align:(fun d b i col -> d b col col)
      ~group:(fun w d b i col ->
          if b && col + w <= width then d false i col else d b i col)
      doc
      true
      0
      0
*)
end

module Stream = struct

  type not_group
  type is_group

  type _ event' =
    | Text : string -> not_group event'
    | Char : char -> not_group event'

    | Start_group : { mutable measure : int } -> is_group event'
    | End_group : not_group event'

    | Break : string -> not_group event'
    | Alignment_spaces : int -> not_group event'

    | Start_nest : int -> not_group event'
    | Start_align : not_group event'
    | End_nestalign : not_group event'

  type event =
    | Ev : _ event' -> event [@@ocaml.unboxed]

  type t =
    { mutable width    : int
    (** The target width, used for making decisions about line
        breaks. *)

    ; output           : Output.t
    (** The output functions. *)

    (* measurement *)
    ; queue            : event Queue.t
    (** Queue of events that are waiting on the resolution of group
              sizes. *)
    ; mutable pos      : int
    (** The absolute position in the input, if everything was flat. *)
    ; open_groups      : is_group event' CCDeque.t
    ; closed_groups    : is_group event' Queue.t

    (* layout state *)
    ; mutable flat     : int (* number of 'start_group's we are
                                      nested in, in flat mode *)
    ; mutable column   : int
    ; mutable indent   : int
    ;         indents  : int Stack.t
    }

  let create ?(width=80) output =
    { queue         = Queue.create ()
    ; pos           = 1 (* count from 1 so we can represent measuring
                           tasks by negative numbers and not get
                           confused by 0-width groups *)
    ; open_groups   = CCDeque.create ()
    ; closed_groups = Queue.create ()
    ; output
    ; width
    ; flat          = 0
    ; column        = 0
    ; indent        = 0
    ; indents       = Stack.create ()
    }

  let stdout =
    create Output.stdout

  let stderr =
    create Output.stderr

  let set_width t new_width =
    t.width <- new_width

  let layout st =
    let open Output in
    (* whenever we get stuck in layout, the head of the queue will
       always be a Start_group, and the start_groups will always be in
       the order they are in the open_ and closed_groups queues.

       This means that we know how far to step through the queue by
       counting the number of start_groups that have been evicted in the
       last overflow check, or now have fixed measures instead of
       dynamically checking them. *)
    let is_live = function
      | Ev (Start_group {measure}) when measure < 0 -> false
      | _ -> true
    in
    while not (Queue.is_empty st.queue) && is_live (Queue.peek st.queue) do
      match Queue.take st.queue with
        | Ev (Text s) ->
           st.output.text s;
           st.column <- st.column + String.length s
        | Ev (Char c) ->
           st.output.char c;
           st.column <- st.column + 1
        | Ev (Start_group _) when st.flat > 0 ->
           st.flat <- st.flat + 1
        | Ev (Start_group {measure}) when st.column + measure <= st.width ->
           st.flat <- 1
        | Ev (Start_group _) ->
           ()
        | Ev End_group ->
           if st.flat > 0 then st.flat <- st.flat - 1
        | Ev (Break s) ->
           if st.flat = 0 then
             (st.output.newline ();
              st.output.spaces st.indent;
              st.column <- st.indent)
           else
             (st.output.text s;
              st.column <- st.column + String.length s)
        | Ev (Alignment_spaces i) ->
           if st.flat = 0 then
             (st.output.spaces i;
              st.column <- st.column + i)
        | Ev (Start_nest i) ->
           Stack.push st.indent st.indents;
           st.indent <- st.indent + i
        | Ev Start_align ->
           Stack.push st.indent st.indents;
           st.indent <- st.column
        | Ev End_nestalign ->
           (match Stack.pop st.indents with
             | exception Stack.Empty ->
                failwith "badly nested nests or aligns"
             | i ->
                st.indent <- i)
    done

  let evict_overflowed_groups st =
    let rec check_closed_groups () =
      match Queue.peek st.closed_groups with
        | exception Queue.Empty ->
           ()
        | Start_group r when st.pos + r.measure > st.width ->
           ignore (Queue.take st.closed_groups);
           r.measure <- st.width + 1;
           check_closed_groups ()
        | Start_group _ ->
           ()
    in
    check_closed_groups ();
    let rec check_open_groups () =
      match CCDeque.peek_front st.open_groups with
        | Start_group r when st.pos + r.measure > st.width ->
           ignore (CCDeque.take_front st.open_groups);
           r.measure <- st.width + 1;
           check_open_groups ()
        | exception CCDeque.Empty ->
           ()
        | _ ->
           ()
    in
    check_open_groups ()
  (* conjecture: all the pruned groups occur in the order they appear
     in the queue, so we can just count the number of evicted groups,
     and step that far forwards in the queue of pending events. This
     would avoid the need for the reference too. TODO: I'm pretty sure
     this isn't true. *)

  let text st txt =
    let width = String.length txt in
    if width > 0 then begin
      (* FIXME: if the item at the head of the queue is a start_group,
         then switch the order. This reduces the latency of the
         printer so that text is always output immediately if we
         aren't waiting for a decision on a line break. *)
      Queue.push (Ev (Text txt)) st.queue;
      st.pos <- st.pos + width;
      evict_overflowed_groups st;
      layout st
    end

  let char st c =
    Queue.push (Ev (Char c)) st.queue;
    st.pos <- st.pos + 1;
    evict_overflowed_groups st;
    layout st

  let start_group st =
    let ev = Start_group { measure = - st.pos } in
    Queue.push (Ev ev) st.queue;
    CCDeque.push_back st.open_groups ev

  let end_group st =
    Queue.push (Ev End_group) st.queue;
    if not (CCDeque.is_empty st.open_groups) then
      let measurer = CCDeque.take_back st.open_groups in
      Queue.push measurer st.closed_groups

  (* [flush_closed_groups] sets the final measurement for all
     unpruned closed groups in the input stream. There may still be
     unfinished open groups, so we cannot yet necessarily print
     these groups. *)
  let flush_closed_groups st =
    while not (Queue.is_empty st.closed_groups) do
      match Queue.take st.closed_groups with
        | Start_group r -> r.measure <- st.pos + r.measure
    done

  let break st txt =
    flush_closed_groups st;
    Queue.push (Ev (Break txt)) st.queue;
    st.pos <- st.pos + String.length txt;
    evict_overflowed_groups st;
    layout st

  let flush st =
    if not (CCDeque.is_empty st.open_groups) then
      invalid_arg "Pretty.finish: open groups at end of input";
    flush_closed_groups st;
    layout st;
    assert (Queue.is_empty st.queue)

  let start_nest st i =
    if i < 0 then invalid_arg "Pretty.nest: negative argument";
    Queue.push (Ev (Start_nest i)) st.queue

  let start_align st =
    Queue.push (Ev Start_align) st.queue

  let end_nest_or_align st =
    Queue.push (Ev End_nestalign) st.queue

  let alignment_spaces st i =
    if i > 0 then
      Queue.push (Ev (Alignment_spaces i)) st.queue

end

type document = Doc.t

type output = Output.t

type prettifier = Stream.t
