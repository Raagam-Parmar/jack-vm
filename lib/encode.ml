open Lib

exception UnexpectedOffset of int
exception PopToConstant of string
exception UnexpectedRepeat of int

module Helper = struct
        let rec many (repeat : int) (elem : 'a) : 'a list = 
                if repeat < 0 then
                        raise (UnexpectedRepeat repeat)
                else if repeat = 0 then
                        []
                else
                        elem :: (many (repeat - 1) elem)
        
        let rec many_more (repeat : int) (elem_list : 'a list) : 'a list = 
                List.flatten (many repeat elem_list)
        
        let assign_i (dest : Ast.destination) (i : int) : string Ast.instruction list = [
                        AInst (string_of_int i);
                        Mnemonics.assign dest (Mnemonics.identity A);
                ]

        let copy (d : Ast.destination) (r : Ast.register) : string Ast.instruction = 
                Mnemonics.assign d (Mnemonics.identity r)
end


module SP = struct
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
        val goto : Vmast.Segment.t -> string Ast.instruction list
        val deref : Vmast.Segment.t -> string Ast.instruction list
        val push : Vmast.Segment.t -> int -> string -> string Ast.instruction list
        val pop : Vmast.Segment.t -> int -> string -> string Ast.instruction list
end = struct
        let goto (seg : Vmast.Segment.t) : string Ast.instruction list = 
                match seg with
                | Argument -> [ AInst "ARG"  ]
                | Local    -> [ AInst "LCL"  ]
                | This     -> [ AInst "THIS" ]
                | That     -> [ AInst "THAT" ]
                | _        -> failwith "Snippets.Segment.goto: unexpected argument"
        
        let deref (seg : Vmast.Segment.t) : string Ast.instruction list = 
                match seg with
                | Argument -> goto Argument @ [ Helper.copy [A] M ]
                | Local    -> goto Local    @ [ Helper.copy [A] M ]
                | This     -> goto This     @ [ Helper.copy [A] M ]
                | That     -> goto That     @ [ Helper.copy [A] M ]
                | _        -> failwith "Snippets.Segment.goto: unexpected argument seg"

        let push_constant (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                let assign = Helper.assign_i [D] i in
                List.concat [
                        assign;
                        SP.deref;
                        [Helper.copy [M] D];
                        SP.incr
                ]
        
        let pop_segment_short (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                let a_insts = Helper.many i (Mnemonics.assign [A] (Mnemonics.succ A)) in

                List.concat [
                        SP.deref_sub; 
                        [ Helper.copy [D] M ]; 
                        deref s; 
                        a_insts; 
                        [ Helper.copy [M] D] 
                ]
        
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
        
        let push_temp (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                List.concat [
                        [ Ast.AInst (string_of_int (5 + i)) ];
                        [ Helper.copy [D] M ];
                        SP.deref;
                        [ Helper.copy [M] D ];
                        SP.incr
                ]
        
        let pop_temp (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        [ Ast.AInst (string_of_int (5 + i)) ];
                        [ Helper.copy [M] D ]
                ]
        
        let push_this_that (s : Vmast.Segment.t) : string Ast.instruction list = 
                List.concat [
                        deref s;
                        [ Helper.copy [D] M ];
                        SP.deref;
                        [ Helper.copy [M] D ];
                        SP.incr;
                ]
        
        let pop_this_that (s : Vmast.Segment.t) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        goto s;
                        [ Helper.copy [M] D ]
                ]
        
        let push_static (file_name : string) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                List.concat [ 
                        [ Ast.AInst (file_name ^ "." ^ string_of_int i) ];
                        [ Helper.copy [D] M ];
                        SP.deref;
                        [ Helper.copy [M] D ];
                        SP.incr
                ]

        let pop_static (file_name : string) (i : Vmast.Instruction.offset) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        [ Ast.AInst (file_name ^ "." ^ string_of_int i) ];
                        [ Helper.copy [M] D ]
                ]
        
        let push (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) (f : string) : string Ast.instruction list = 
                if (i < 0) then raise (UnexpectedOffset i) else
                match s with
                | Argument  ->  push_segment s i
                | Local     ->  push_segment s i
                | Static    ->  push_static f i
                | Constant  ->  push_constant i
                | This      ->  push_this_that s
                | That      ->  push_this_that s
                | Pointer   ->  if (i == 0) then 
                                push_this_that This 
                                else if (i == 1) then 
                                push_this_that That 
                                else raise (UnexpectedOffset i)
                | Temp      ->  push_temp i
        
        let pop (s : Vmast.Segment.t) (i : Vmast.Instruction.offset) (f : string) : string Ast.instruction list = 
                if (i < 0) then raise (UnexpectedOffset i) else
                match s with
                | Argument  ->  if (i <= 5) then pop_segment_short s i else pop_segment_long s i
                | Local     ->  if (i <= 5) then pop_segment_short s i else pop_segment_long s i
                | Static    ->  pop_static f i
                | Constant  ->  raise (PopToConstant "Cannot pop to Constant")
                | This      ->  pop_this_that s
                | That      ->  pop_this_that s
                | Pointer   ->  if (i == 0) then 
                                pop_this_that This 
                                else if (i == 1) then 
                                pop_this_that That 
                                else raise (UnexpectedOffset i)
                | Temp      ->  pop_temp i

end


module Binary : sig
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
        val annd : string Ast.instruction list
        val orr : string Ast.instruction list
        val not : string Ast.instruction list
end = struct
        let annd = Binary.v (Mnemonics.binAnd)
        let orr  = Binary.v (Mnemonics.binOr)
        let not  = Unary.v (Mnemonics.bitNot)

end

module Arithmetic : sig
        val add : string Ast.instruction list
        val sub : string Ast.instruction list
        val neg : string Ast.instruction list
end = struct
        let add = Binary.v (Mnemonics.add)
        let sub = Binary.v (Mnemonics.subFrom)
        let neg = Unary.v (Mnemonics.minus)
end

module Compare : sig
        val eq : string Ast.instruction list
        val gt : string Ast.instruction list
        val lt : string Ast.instruction list
end = struct     
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
        val v : string -> string -> string
end = struct
        let v (l : string) (f : string) : string = 
                let label = Printf.sprintf "%s$%s" l f in 
                label

end


module Label : sig
        val encode : string -> string -> string Ast.instruction list
end = struct
        let encode (l : string) (f : string) : string Ast.instruction list = 
                [ Ast.Label (UniqueLabel.v l f) ]
end


module Goto : sig
        val encode : string -> string -> string Ast.instruction list
end = struct
        let encode (l : string) (f : string) : string Ast.instruction list = [
                AInst (UniqueLabel.v l f); 
                Mnemonics.jmp
        ]
end


module IfGoto : sig
        val encode : string -> string -> string Ast.instruction list
end = struct
        let encode (l : string) (f : string) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        [ Ast.AInst (UniqueLabel.v l f) ];
                        [ Mnemonics.jne ]
                ]
end


module Call : sig
        val v : string -> Vmast.Instruction.nArgs -> string -> string Ast.instruction list
        val save : string -> Vmast.Instruction.nArgs -> string Ast.instruction list
end = struct
        let return_address (addr : string) : string Ast.instruction list = 
                List.concat [
                        [ Ast.AInst addr ];
                        [ Helper.copy [D] A ];
                        SP.deref;
                        [ Helper.copy [M] D ];
                        SP.incr
                ]
                
        let segment (seg : Vmast.Segment.t) : string Ast.instruction list = 
                List.concat [
                        Segment.goto seg;
                        [ Helper.copy [D] M ];
                        SP.deref;
                        [ Helper.copy [M] D ];
                        SP.incr
                ]
        
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
        val v : string -> int -> string Ast.instruction list
end = struct
        let v (fName : string) (nArgs : Vmast.Instruction.nArgs) : string Ast.instruction list = 
                List.concat [
                        [ Ast.Label fName ];
                        Helper.many_more nArgs (Segment.push Constant 0 "")
                ]

end


module Return : sig
        val v : string Ast.instruction list
end = struct
        let set_frame : string Ast.instruction list = 
                List.concat [
                        Segment.goto Local;
                        [ Helper.copy [D] M ];
                        [ AInst "R13" ]; (* TODO: MAKE THIS MODULAR, @R13 *)
                        [ Helper.copy [M] D]
                ]
        
        let set_ret : string Ast.instruction list = 
                List.concat [
                        [ Ast.AInst "5" ];
                        [ Mnemonics.assign [A] (Mnemonics.sub A) ];
                        [ Helper.copy [D] M ];
                        [ Ast.AInst "R14" ];
                        [ Helper.copy [M] D ]
                ]
        
        let arg_pop : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        Segment.goto Argument;
                        [ Helper.copy [A] M ];
                        [ Helper.copy [M] D ]
                ]

        let set_sp : string Ast.instruction list = 
                List.concat [
                        Segment.goto Argument;
                        [ Mnemonics.assign [D] (Mnemonics.succ M) ];
                        [ SP.goto ];
                        [ Helper.copy [M] D ]
                ]
        
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
        val generate : unit -> string
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
        val encode : (string, string) Vmast.Instruction.t -> string  -> string -> string Ast.instruction list
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
        val encode : (string, string) Vmast.Instruction.t list -> string -> string -> string Ast.instruction list
end = struct
        let rec encode (insts : (string, string) Vmast.Instruction.t list) (funcName : string) (fileName : string) : string Ast.instruction list = 
                match insts with
                | []      -> []
                | i :: is -> Instructions.encode i funcName fileName @ encode is funcName fileName
end


module Function : sig
        val encode : (string, string) Vmast.Function.t -> string -> string Ast.instruction list
end = struct
        let encode (func : (string, string) Vmast.Function.t) (fileName : string) : string Ast.instruction list = 
                match func with
                | { name; nVars; body } -> 
                        List.concat [
                                Preamble.v name nVars;
                                Body.encode body name fileName
                        ]
end

module Bootstrap : sig
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
        val encode : (string, string) Vmast.Program.t -> string -> string Ast.program
end = struct
        let rec no_bootstrap_encode (p : (string, string) Vmast.Program.t) (fileName : string) : string Ast.instruction list =                 
                match p with
                | []      -> []
                | f :: fs -> Function.encode f fileName @ no_bootstrap_encode fs fileName
        
        let encode (p : (string, string) Vmast.Program.t) (fileName : string) : string Ast.instruction list = 
                Bootstrap.v @
                no_bootstrap_encode p fileName
end