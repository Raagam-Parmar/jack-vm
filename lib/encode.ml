open Lib

exception UnexpectedOffset of (Vmast.Segment.t * int)
exception PopToConstant of string
exception UnexpectedRepeat of int
exception UnexpectedSegment of Vmast.Segment.t

module Helper : sig 
        (** [many r e] repeats [e] in a list [r] number of times, raising [UnexpectedRepeat r] if [r] is negative *)
        val many : int -> 'a -> 'a list

        (** [many_more r el] repeats list [el] in a flattened list [r] number of times, raising [UnexpectedRepeat r] if [r] is negative *)
        val many_more : int -> 'a list -> 'a list

        (** [asssign_i dest i] assigns the integer value [i] to destination(s) [dest] *)
        val assign_i : Ast.destination -> int -> string Ast.instruction list

        (** [copy dest r] copies the memory value in register [r] and assigns it to destination(s) [dest] *)
        val copy : Ast.destination -> Ast.register -> string Ast.instruction
end = struct
        let rec many (repeat : int) (elem : 'a) : 'a list = 
                if repeat < 0 then
                        raise (UnexpectedRepeat repeat)
                else if repeat = 0 then
                        []
                else
                        elem :: (many (repeat - 1) elem)
        
        let many_more (repeat : int) (elem_list : 'a list) : 'a list = 
                List.flatten (many repeat elem_list)
        
        let assign_i (dest : Ast.destination) (i : int) : string Ast.instruction list = [
                        AInst (string_of_int i);
                        Mnemonics.assign dest (Mnemonics.identity A);
                ]

        let copy (d : Ast.destination) (r : Ast.register) : string Ast.instruction = 
                Mnemonics.assign d (Mnemonics.identity r)
end


module SP : sig 
        (** [goto] produces ASM instruction which translates to [@SP] *)
        val goto : string Ast.instruction

        (** [deref] produces ASM instructions which translate to [@SP; A=M], effectively making A-register point to 
        the memory location given in [SP] *)
        val deref : string Ast.instruction list

        (** [deref_sub] is similar to [deref], except it decerases [SP] and points to one memory-address below/lesser than [SP]*)
        val deref_sub : string Ast.instruction list

        (** [incr] increments [SP] by one *)
        val incr : string Ast.instruction list

        (** [decr] decrements [SP] by one *)
        val decr : string Ast.instruction list

        (** [assignD] assigns the value in D-register to [*SP] (dereferenced SP) *)
        val assignD : string Ast.instruction list
end = struct
        let goto : string Ast.instruction = 
                AInst "SP"
        
        let deref : string Ast.instruction list = [
                goto; 
                Mnemonics.assign [A] (Mnemonics.identity M) 
        ]
        
        let deref_sub : string Ast.instruction list = [
                goto; 
                Mnemonics.assign [A; M] (Mnemonics.pred M) 
        ]

        let incr : string Ast.instruction list = [
                goto; 
                Mnemonics.assign [M] (Mnemonics.succ M) 
        ]
        
        let decr : string Ast.instruction list = [
                goto; 
                Mnemonics.assign [M] (Mnemonics.pred M) 
        ]

        let assignD : string Ast.instruction list = 
                deref @ [ Mnemonics.assign [M] (Mnemonics.identity D) ]
end


module Segment : sig
        (** [goto seg] generates ASM instructions which translate to [@seg].
        For instance, [goto Local] will translate into [@LCL].
        It raises [UnexpectedSegment seg] if [seg] is not
                1. Argument
                2. Local
                3. This
                4. That 
                *)
        val goto : Vmast.Segment.t -> string Ast.instruction list

        (** [deref seg] generates ASM instructions which translate to [@seg; A=M].
        They effectively point to the memory address stored in [seg] 
        It raises [UnexpectedSegment seg] if [seg] is not
                1. Argument
                2. Local
                3. This
                4. That *)
        val deref : Vmast.Segment.t -> string Ast.instruction list

        (** [push seg n] pushes onto the VM stack, the value at offset [n] from the base-pointer of [seg] *)
        val push : Vmast.Segment.t -> int -> string -> string Ast.instruction list

        (** [pop seg n] pops from the VM stack, into offset [n] from the base-pointer of [seg].
        It raises [PopToConstant] if VM tries to pop a value into the [Constant] segment. *)
        val pop : Vmast.Segment.t -> int -> string -> string Ast.instruction list
end = struct
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
end


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


module Call : sig
        (** [v func n addr] calls function [f] with [n] arguments using [addr] as the unique return address. *)
        val v : string -> Vmast.Instruction.nArgs -> string -> string Ast.instruction list

        (** [save addr n] generated ASM instructions to save the frame of the calling-function, which is calling 
        a function with [n] arguments. *)
        val save : string -> Vmast.Instruction.nArgs -> string Ast.instruction list
end = struct
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


module Return : sig
        (** [v] generates the ASM instructions which return the control flow form callee back to the caller. *)
        val v : string Ast.instruction list
end = struct
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
        
        
end


module ReturnAddress : sig
        (** [generate ()] generates a unique return address every time it is called. It is of the form
        ["RET_ADDRESS_CALLi"] where integer [i] is unique every time this function is called. *)
        val generate : unit -> string

        (** [bootstrap] is the string which is used for the Bootstrap code under [Bootstrap.v] *)
        val bootstrap : string
end = struct
        let address_counter = ref 1

        let prefix = "RET_ADDRESS_CALL"

        let generate () : string = 
                let address = Printf.sprintf "%s%d" prefix !address_counter in
                incr address_counter;
                address

        let bootstrap = Printf.sprintf "%s%d" prefix 0 
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
                | Ret                 -> Return.v
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

                Call.v "Sys.init" 0 ReturnAddress.bootstrap
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