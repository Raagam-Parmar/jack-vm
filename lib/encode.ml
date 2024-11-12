open Lib

exception UnexpectedOffset of (Vmast.Segment.t * int)
exception PopToConstant of string
exception UnexpectedRepeat of int
exception UnexpectedSegment of Vmast.Segment.t

module Binary : sig
    (** [v f] generates ASM instructions which correspond to the binary operation done by [f]. [f] comes form [Mnemonics] module of the Assembler. *)
    val v : (Ast.register -> Ast.computation) -> string Ast.instruction list
end = struct
    let v (f : Ast.register -> Ast.computation) : string Ast.instruction list = 
        List.concat [
            SP.deref_sub;
            [ Helper.copy [D] M ];
            [ Mnemonics.assign [A] (Mnemonics.pred A) ];
            [ Mnemonics.assign [M] (f M) ]
        ]
end

module Unary : sig
    (** [v f] generates ASM instructions which correspond to the unary operation done by [f]. [f] comes form [Mnemonics] module of the Assembler. *)
    val v : (Ast.register -> Ast.computation) -> string Ast.instruction list
end = struct
    let v (f : Ast.register -> Ast.computation) : string Ast.instruction list = 
        List.concat [
            [ SP.goto ];
            [ Mnemonics.assign [A] (Mnemonics.pred M) ];
            [ Mnemonics.assign [M] (f M) ]
        ]
end


module Bitwise : sig
    (** [annd] pops two values form the VM stack, [y], then [x], and pushes [x ∧ y] onto the stack. *)
    val annd : string Ast.instruction list

    (** [orr] pops two values form the VM stack, [y], then [x], and pushes [x ∨ y] onto the stack. *)
    val orr : string Ast.instruction list

    (** [not] pops one value [x] form the VM stack and pushes [¬ x] onto the stack. *)
    val not : string Ast.instruction list
end = struct
    let annd = Binary.v (Mnemonics.binAnd)
    let orr  = Binary.v (Mnemonics.binOr)
    let not  = Unary.v (Mnemonics.bitNot)

end

module Arithmetic : sig
    (** [add] pops two values form the VM stack, [y], then [x], and pushes [x + y] onto the stack. *)
    val add : string Ast.instruction list

    (** [sub] pops two values form the VM stack, [y], then [x], and pushes [x - y] onto the stack. *)
    val sub : string Ast.instruction list

    (** [neg] pops one value [x] form the VM stack and pushes [- x] onto the stack. *)
    val neg : string Ast.instruction list
end = struct
    let add = Binary.v (Mnemonics.add)
    let sub = Binary.v (Mnemonics.subFrom)
    let neg = Unary.v (Mnemonics.minus)
end

module Compare : sig
    (** [eq] pops two binary values form the VM stack, [y], then [x], and pushes [x == y] onto the stack. *)
    val eq : string Ast.instruction list

    (** [gt] pops two binary values form the VM stack, [y], then [x], and pushes [x > y] onto the stack. *)
    val gt : string Ast.instruction list

    (** [lt] pops two binary values form the VM stack, [y], then [x], and pushes [x < y] onto the stack. *)
    val lt : string Ast.instruction list
end = struct
        (** [compare j] generates ASM instructions which correspond to the jump condition giveby [j]. *)
    let compare (j : Ast.jump) : string Ast.instruction list = 
        let if_condition = [ Ast.Ref 7; Mnemonics.jumpOf j ] in
        let true_body = [
            SP.goto; 
            Mnemonics.assign [A] (Mnemonics.pred M); 
            Mnemonics.assign [M] (Ast.Constant MinusOne) 
        ] 

        in let false_body = [ 
            SP.goto; 
            Mnemonics.assign [A] (Mnemonics.pred M); 
            Mnemonics.assign [M] (Ast.Constant Zero); 
            Ast.Ref 5; 
            Mnemonics.jmp 
        ]

        in List.concat [
            SP.deref_sub;
            [ Helper.copy [D] M ];
            [ Mnemonics.assign [A] (Mnemonics.pred A) ];
            [ Mnemonics.assign [D] (Mnemonics.subFrom M) ];

            if_condition;
            false_body;
            true_body
        ]

    let eq = compare (Ast.JEQ)
    let gt = compare (Ast.JGT)
    let lt = compare (Ast.JLT)
end


module UniqueLabel : sig
    (** [v l f] generates a unique label for a function named [f], which looks like ["f$l"].
    For example, if we [label IF_TRUE] inside function [Main.main], then the unique label
    would be ["Main.main$IF_TRUE"]. This affects the scope of the label which is limited
    within the function [f] *)
    val v : string -> string -> string
end = struct
    let v (l : string) (f : string) : string = 
        let label = Printf.sprintf "%s$%s" f l in 
        label
end


module Label : sig
    (** [encode l f] uses [UniqueLabel.v] to translate [Label l] into ASM instructions. *)
    val encode : string -> string -> string Ast.instruction list
end = struct
    let encode (l : string) (funcName : string) : string Ast.instruction list = 
        [ Ast.Label (UniqueLabel.v l funcName) ]
end


module Goto : sig
    (** [encode l f] uses [UniqueLabel.v] to translate [Goto l] into ASM instructions. *)
    val encode : string -> string -> string Ast.instruction list
end = struct
    let encode (l : string) (funcName : string) : string Ast.instruction list = [
        AInst (UniqueLabel.v l funcName); 
        Mnemonics.jmp
    ]
end


module IfGoto : sig
    (** [encode l f] uses [UniqueLabel.v] to translate [IfGoto l] into ASM instructions. *)
    val encode : string -> string -> string Ast.instruction list
end = struct
    let encode (l : string) (funcName : string) : string Ast.instruction list = 
        List.concat [
            SP.deref_sub;
            [ Helper.copy [D] M ];
            [ Ast.AInst (UniqueLabel.v l funcName) ];
            [ Mnemonics.jne ]
        ]
end


module Preamble : sig
    (** [v f n] generates preambble for the function [f] which has [n] local variables by 
    doing [Push Constant 0], [n] number of times. *)
    val v : string -> int -> string Ast.instruction list
end = struct
    let v (funName : string) (nArgs : Vmast.Instruction.nArgs) : string Ast.instruction list = 
        List.concat [
                [ Ast.Label funName ];
                Helper.many_more nArgs (Segment.push Constant 0 "")
        ]
end


module Instructions : sig
    (** [encode i fun file] translates VM instruction [i] of function name [fun] within a VM file [file.vm]. *)
    val encode : (string, string) Vmast.Instruction.t -> string -> string -> string Ast.instruction list
end = struct
    let encode (i : (string, string) Vmast.Instruction.t) (funcName : string) (fileName : string) : string Ast.instruction list = 
        match i with
        | Push (seg, nArgs) -> Segment.push seg nArgs fileName
        | Pop  (seg, nArgs) -> Segment.pop  seg nArgs fileName
        
        | Add -> Arithmetic.add
        | Sub -> Arithmetic.sub
        | Neg -> Arithmetic.neg

        | Eq  -> Compare.eq
        | Gt  -> Compare.gt
        | Lt  -> Compare.lt

        | And -> Bitwise.annd
        | Or  -> Bitwise.orr
        | Not -> Bitwise.not

        | Label l  -> Label.encode l funcName
        | Goto l   -> Goto.encode l funcName
        | IfGoto l -> IfGoto.encode l funcName

        | Call (funcName, nArgs) -> Call.v funcName nArgs (ReturnAddress.generate ())  
        | Ret                    -> Return.v
end


module Body : sig
    (** [encode il fun file] translates VM instruction list [il] of function name [fun] within a VM file [file.vm]. *)
    val encode : (string, string) Vmast.Instruction.t list -> string -> string -> string Ast.instruction list
end = struct
    let rec encode (insts : (string, string) Vmast.Instruction.t list) (funcName : string) (fileName : string) : string Ast.instruction list = 
        match insts with
        | []      -> []
        | i :: is -> Instructions.encode i funcName fileName @ encode is funcName fileName
end


module Function : sig
    (** [encode f file] translates VM Function [f] within a VM file [file.vm]. *)
    val encode : string -> (string, string) Vmast.Function.t -> string Ast.instruction list
end = struct
    let encode (fileName : string) (func : (string, string) Vmast.Function.t) : string Ast.instruction list = 
        match func with
        | { name; nVars; body } -> 
            List.concat [
                Preamble.v name nVars;
                Body.encode body name fileName
            ]
end


module Bootstrap : sig
    (** [v] generates ASM code for the bootstrapping the VM. It is necessary for the VM to have a [Sys.init] function. *)
    val v : string Ast.instruction list
end = struct
    let v : string Ast.instruction list = List.concat [
            [ Ast.AInst "256" ];
            [ Helper.copy [D] A ];
            
            [ SP.goto ];
            [ Helper.copy [M] D];

            Call.bootstrap
        ]
end


module Program : sig
    (** [encode file p] bootstraps and translates the VM program [p] which is written in a file [file.vm]. *)
    val encode : string -> (string, string) Vmast.Program.t -> string Ast.program
end = struct
        (** [no_bootstrap_encode p file] doesn't bootstrap and translates the VM program [p] which is written in a file [file.vm]. *)
    let rec no_bootstrap_encode (fileName : string) (p : (string, string) Vmast.Program.t) : string Ast.instruction list =                 
        match p with
        | []      -> []
        | f :: fs -> Function.encode fileName f @ no_bootstrap_encode fileName fs
    
    let encode (fileName : string) (p : (string, string) Vmast.Program.t) : string Ast.instruction list = 
        Bootstrap.v @
        no_bootstrap_encode fileName p
end