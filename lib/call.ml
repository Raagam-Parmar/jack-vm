open Lib

(** [return_address addr] saves the unique return address of the caller, given by [addr]. *)
let return_address (addr : string) : string Ast.instruction list = 
    List.concat [
        [ Ast.AInst addr ];
        [ Helper.copy [D] A ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr
]
            
(** [segment seg] saves the segment [seg] of the caller. *)
let segment (seg : Vmast.Segment.t) : string Ast.instruction list = 
    List.concat [
        Segment.goto seg;
        [ Helper.copy [D] M ];
        SP.deref;
        [ Helper.copy [M] D ];
        SP.incr
]
    
(** [setup_arg n] sets up the argument section for the callee with [n] arguments. *)
let setup_arg (n : Vmast.Instruction.nArgs) : string Ast.instruction list = 
    List.concat [
        [ SP.goto ];
        [ Helper.copy [D] M ];
        [ AInst "5" ];
        [ Mnemonics.assign [D] (Mnemonics.sub A) ];
        [ AInst (string_of_int n) ];
        [ Mnemonics.assign [D] (Mnemonics.sub A) ];
        Segment.goto Argument;
        [ Helper.copy [M] D ]
    ]

(** [setup_lcl] sets up the Local segment for the callee. *)
let setup_lcl : string Ast.instruction list = 
    List.concat [
        [ SP.goto ];
        [ Helper.copy [D] M ];
        Segment.goto Local;
        [ Helper.copy [M] D]
    ]

let save (addr : string) (n : Vmast.Instruction.nArgs) : string Ast.instruction list = 
    List.concat [
        return_address addr;
        segment Local;
        segment Argument;
        segment This;
        segment That;
        setup_arg n ;
        setup_lcl;
    ]

let v (fName : string) (nArgs : Vmast.Instruction.nArgs) (addr : string) : string Ast.instruction list = 
    List.concat [
        save addr nArgs;
        [ AInst fName ];
        [ Mnemonics.jmp ];
        [ Label addr ]
    ]

let bootstrap : string Ast.instruction list = 
    List.concat [
        [ Ast.AInst "Sys.init" ];
        [ Mnemonics.jmp ];
        [ Label ReturnAddress.bootstrap ]
    ]