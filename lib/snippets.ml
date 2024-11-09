open Lib

exception UnexpectedOffset of int
exception PopToConstant of string

module Helper = struct
        let rec many (repeat : int) (inst : string Ast.instruction) : string Ast.instruction list = 
                if repeat < 0 then
                        failwith "Snippets.many: unexpected argument repeat"
                else if repeat = 0 then
                        []
                else
                        inst :: (many (repeat - 1) inst)
        
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
                List.concat [assign ; SP.deref; [Helper.copy [M] D] ; SP.incr]
        
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

module Arithmetic : sig
        val vm_add : string Ast.instruction list
        val vm_sub : string Ast.instruction list
        val vm_and : string Ast.instruction list
        val vm_or : string Ast.instruction list

        val vm_neg : string Ast.instruction list
        val vm_not : string Ast.instruction list

        val vm_eq : string Ast.instruction list
        val vm_gt : string Ast.instruction list
        val vm_lt : string Ast.instruction list
end = struct
        let binary (f : Ast.register -> Ast.computation) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Helper.copy [D] M ];
                        [ Mnemonics.assign [A] (Mnemonics.pred A) ];
                        [ Mnemonics.assign [M] (f M) ]
                ]

        let unary (f : Ast.register -> Ast.computation) : string Ast.instruction list = 
                List.concat [
                        SP.deref_sub;
                        [ Mnemonics.assign [M] (f M) ]
                ]
        
        let compare (j : Ast.jump) : string Ast.instruction list = 
                let if_condition = [ Ast.Ref 7; Mnemonics.jeq ] in
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
                        Mnemonics.jumpOf j 
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
        
        let vm_add = binary (Mnemonics.add)
        let vm_sub = binary (Mnemonics.subFrom)
        let vm_and = binary (Mnemonics.binAnd)
        let vm_or = binary (Mnemonics.binOr)
        let vm_neg = unary (Mnemonics.minus)
        let vm_not = unary (Mnemonics.bitNot)

        let vm_eq = compare (Ast.JEQ)
        let vm_gt = compare (Ast.JGT)
        let vm_lt = compare (Ast.JLT)
end