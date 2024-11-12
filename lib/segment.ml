open Lib

exception UnexpectedOffset of (Vmast.Segment.t * int)
exception UnexpectedSegment of Vmast.Segment.t
exception PopToConstant of string


let goto (seg : Vmast.Segment.t) : string Ast.instruction list = 
    match seg with
    | Argument -> [ AInst "ARG"  ]
    | Local    -> [ AInst "LCL"  ]
    | This     -> [ AInst "THIS" ]
    | That     -> [ AInst "THAT" ]
    | _        -> raise (UnexpectedSegment seg)

let deref (seg : Vmast.Segment.t) : string Ast.instruction list = 
    match seg with
    | Argument -> goto Argument @ [ Helper.copy [A] M ]
    | Local    -> goto Local    @ [ Helper.copy [A] M ]
    | This     -> goto This     @ [ Helper.copy [A] M ]
    | That     -> goto That     @ [ Helper.copy [A] M ]
    | _        -> raise (UnexpectedSegment seg)

(** [push_constant i] pushes constant int [i] onto the VM stack *)
let push_constant (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    let assign = Helper.assign_i [D] i in
    List.concat [
        assign;
        SP.deref;
        [Helper.copy [M] D];
        SP.incr
    ]

(** [pop_segment_short s i] pops from the VM stack, into offset [i] of segment [s]. This is used when 0 <= [i] <= 5 *)
let pop_segment_short (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    let a_insts = Helper.many i (Mnemonics.assign [A] (Mnemonics.succ A)) in

    List.concat [
        SP.deref_sub; 
        [ Helper.copy [D] M ]; 
        deref s; 
        a_insts; 
        [ Helper.copy [M] D] 
    ]

(** [pop_segment_short s i] pops from the VM stack, into offset [i] of segment [s]. This is used when [i] >= 6 *)
let pop_segment_long (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    let assign = Helper.assign_i [D] i in
    let set_d  = goto s @ [ Mnemonics.assign [D] (Mnemonics.add M) ] in
    let copy_to_temp = [ Ast.AInst "5"; Helper.copy [M] D ] in
    let copy_from_temp = [ Ast.AInst "5"; Helper.copy [A] M ] in

    List.concat [
        assign;
        set_d;
        copy_to_temp;
        SP.deref_sub;
        [ Helper.copy [D] M ];
        copy_from_temp;
        [ Helper.copy [M] D ]
    ]

(** [push_segment s i] pushes onto the VM stack, the value at offset [i] in segment [s] *)
let push_segment (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    let assign = Helper.assign_i [D] i in
    let set_a = goto s @ [ Mnemonics.assign [A] (Mnemonics.add M) ] in
    
    List.concat [
        assign;
        set_a;
        [ Helper.copy [D] M ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr;
    ]

(** [push_temp i] pushes onto the VM stack, the value at offset 0 <= [i] <= 7 of segment [Temp]. 
Raises [UnexpectedOffset] if [i] is not in range. *)
let push_temp (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if i >= 8 then raise (UnexpectedOffset (Temp, i))
    else
    List.concat [
        [ Ast.AInst (string_of_int (5 + i)) ];
        [ Helper.copy [D] M ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr
    ]

(** [pop_temp i] pops from the VM stack, into the offset 0 <= [i] <= 7 of segment [Temp]. 
Raises [UnexpectedOffset] if [i] is not in range. *)
let pop_temp (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if i >= 8 then raise (UnexpectedOffset (Temp, i))
    else
    List.concat [
        SP.deref_sub;
        [ Helper.copy [D] M ];
        [ Ast.AInst (string_of_int (5 + i)) ];
        [ Helper.copy [M] D ]
    ]

(** [push_this_that s] is same as [push s], there [s] can only be either [This] or [That] *)
let push_this_that (s : Vmast.Segment.t) : string Ast.instruction list = 
    List.concat [
        deref s;
        [ Helper.copy [D] M ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr;
    ]

(** [pop_this_that s] is same as [pop s], there [s] can only be either [This] or [That] *)
let pop_this_that (s : Vmast.Segment.t) : string Ast.instruction list = 
    List.concat [
        SP.deref_sub;
        [ Helper.copy [D] M ];
        goto s;
        [ Helper.copy [M] D ]
    ]

(** [push_static fileName i] pushes onto the VM stack, the value at offset 0 <= [i] <= 239 in segment [Static], for a file named [fileName.vm]
Raises [UnexpectedOffset] if [i] is not in range. *)
let push_static (fileName : string) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if i >= 240 then raise (UnexpectedOffset (Static, i))
    else
    List.concat [ 
        [ Ast.AInst (fileName ^ "." ^ string_of_int i) ];
        [ Helper.copy [D] M ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr
    ]

(** [pop_static fileName i] pops from the VM stack, into offset 0 <= [i] <= 239 of segment [Static], for a file named [fileName.vm]. 
Raises [UnexpectedOffset] if [i] is not in range. *)
let pop_static (fileName : string) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if i >= 240 then raise (UnexpectedOffset (Static, i))
    else
    List.concat [
        SP.deref_sub;
        [ Helper.copy [D] M ];
        [ Ast.AInst (fileName ^ "." ^ string_of_int i) ];
        [ Helper.copy [M] D ]
    ]

(** [push_pointer i] translates to [push This] if [i]=0, [push That] if [i]=1, and raises [UnexpectedOffset] otherwise *)
let push_pointer (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if (i >= 2) then raise (UnexpectedOffset (Pointer, i))
    else if (i == 0) then push_this_that This
    else push_this_that That

(** [pop_pointer i] translates to [pop This] if [i]=0, [pop That] if [i]=1, and raises [UnexpectedOffset] otherwise *)
let pop_pointer (i : Vmast.Instruction.offset) : string Ast.instruction list = 
    if (i >= 2) then raise (UnexpectedOffset (Pointer, i))
    else if (i == 0) then pop_this_that This
    else pop_this_that That

let push (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) (f : string) : string Ast.instruction list = 
    if (i < 0) then raise (UnexpectedOffset (s, i)) else
    match s with
    | Argument  ->  push_segment s i
    | Local     ->  push_segment s i
    | Static    ->  push_static f i
    | Constant  ->  push_constant i
    | This      ->  push_this_that s
    | That      ->  push_this_that s
    | Pointer   ->  push_pointer i
    | Temp      ->  push_temp i

let pop (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) (f : string) : string Ast.instruction list = 
    if (i < 0) then raise (UnexpectedOffset (s, i)) else
    match s with
    | Argument  ->  if (i <= 5) then pop_segment_short s i else pop_segment_long s i
    | Local     ->  if (i <= 5) then pop_segment_short s i else pop_segment_long s i
    | Static    ->  pop_static f i
    | Constant  ->  raise (PopToConstant "Cannot pop to Segment Constant")
    | This      ->  pop_this_that s
    | That      ->  pop_this_that s
    | Pointer   ->  pop_pointer i
    | Temp      ->  pop_temp i
