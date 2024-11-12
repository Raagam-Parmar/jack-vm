open Lib

(** [ret_frame] sets up a temporary variable [FRAME].
It roughly translates into [FRAME = LCL]. *)
let set_frame : string Ast.instruction list = 
    List.concat [
        Segment.goto Local;
        [ Helper.copy [D] M ];
        [ AInst "R13" ]; (* TODO: MAKE THIS MODULAR, @R13 *)
        [ Helper.copy [M] D]
    ]

(** [set_ret] puts the return address into the temporary variable [RET].
It roughly translates into [RET = *(FRAME-5)] *)
let set_ret : string Ast.instruction list = 
    List.concat [
        [ Ast.AInst "5" ];
        [ Mnemonics.assign [A] (Mnemonics.sub A) ];
        [ Helper.copy [D] M ];
        [ Ast.AInst "R14" ];
        [ Helper.copy [M] D ]
    ]

(** [arg_pop] repositions the return value for the caller. It roughly translates into [*ARG=pop()] *)
let arg_pop : string Ast.instruction list = 
    List.concat [
        SP.deref_sub;
        [ Helper.copy [D] M ];
        Segment.goto Argument;
        [ Helper.copy [A] M ];
        [ Helper.copy [M] D ]
    ]

(** [set_sp] restores [SP] of the caller. It roughly translates into [SP=ARG+1] *)
let set_sp : string Ast.instruction list = 
    List.concat [
        Segment.goto Argument;
        [ Mnemonics.assign [D] (Mnemonics.succ M) ];
        [ SP.goto ];
        [ Helper.copy [M] D ]
    ]

(** [restore_segment seg i] restores segment [seg] which is [i] addresses to the back of [FRAME]. *)
let restore_segment (seg : Vmast.Segment.t) (i : int) : string Ast.instruction list = 
    List.concat [
        [ Ast.AInst "R13" ];
        [ Helper.copy [D] M ];
        [ Ast.AInst (string_of_int i) ];
        [ Mnemonics.assign [A] (Mnemonics.sub A) ];
        [ Helper.copy [D] M ];
        Segment.goto seg;
        [ Helper.copy [M] D ]
    ]

(** [goto_ret] makes the control flow jump back to the caller. *)
let goto_ret : string Ast.instruction list = 
    List.concat [
        [ Ast.AInst "R14" ];
        [ Helper.copy [A] M ];
        [ Mnemonics.jmp ]
    ]

let v : string Ast.instruction list = 
    List.concat [
        set_frame;
        set_ret;
        arg_pop;
        set_sp;
        restore_segment That 1;
        restore_segment This 2;
        restore_segment Argument 3;
        restore_segment Local 4;
        goto_ret
    ]
