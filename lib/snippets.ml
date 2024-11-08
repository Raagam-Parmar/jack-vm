
module SP = struct
        let goto : string Lib.Ast.instruction = 
                AInst "SP"
        
        let deref : string Lib.Ast.instruction list = 
                [goto; Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M)]

        let incr : string Lib.Ast.instruction list = 
                [
                        goto;
                        Lib.Mnemonics.assign [M] (Lib.Mnemonics.succ M)
                ]
        
        let decr : string Lib.Ast.instruction list = 
                [
                        goto;
                        Lib.Mnemonics.assign [M] (Lib.Mnemonics.pred M)
                ]

        let assignD : string Lib.Ast.instruction list = 
                deref @ [Lib.Mnemonics.assign [M] (Lib.Mnemonics.identity D)]
        
end

module Segment = struct
        let goto (seg : Ast.Segment.t) : string Lib.Ast.instruction = 
                match seg with
                | Argument -> AInst "ARG"
                | Local    -> AInst "LCL"
                | This     -> AInst "THIS"
                | That     -> AInst "THAT"
                | _        -> failwith "Snippets.Segment.goto: unexpected argument"
        
        let deref (seg : Ast.Segment.t) : string Lib.Ast.instruction list = 
                match seg with
                | Argument -> [AInst "ARG"; Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M)]
                | Local    -> [AInst "LCL"; Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M)]
                | This     -> [AInst "THIS"; Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M)]
                | That     -> [AInst "THAT"; Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M)]
                | _        -> failwith "Snippets.Segment.goto: unexpected argument seg"
        
end

let rec many (repeat : int) (inst : string Lib.Ast.instruction) : string Lib.Ast.instruction list = 
        if repeat < 0 then
                failwith "Snippets.many: unexpected argument repeat"
        else if repeat = 0 then
                []
        else
                inst :: (many (repeat - 1) inst)

let assign_i (dest : Lib.Ast.destination) (i : int) : string Lib.Ast.instruction list = 
        [
                AInst (string_of_int i);
                Lib.Mnemonics.assign dest (Lib.Mnemonics.identity A);
        ]

let push_constant (offset : int) : string Lib.Ast.instruction list = 
        assign_i [D] offset @ 
        SP.assignD @
        SP.incr

let push (seg : Ast.Segment.t) (offset : int) : string Lib.Ast.instruction list = 
        match seg with
        | Ast.Segment.Constant -> push_constant offset
        | _                    -> 
                assign_i [D] offset
                @ [
                        Segment.goto seg;
                        Lib.Mnemonics.assign [A] (Lib.Mnemonics.add M);
                        Lib.Mnemonics.assign [D] (Lib.Mnemonics.identity M);
                ]
                @ SP.assignD
                @ SP.incr
        
let pop (seg : Ast.Segment.t) (offset : int) : string Lib.Ast.instruction list = 
        if offset < 0 then
                failwith ("Unexpected value of i: " ^ (string_of_int offset))
        else if offset <= 7 then
                [
                        SP.goto;
                        Lib.Mnemonics.assign [A; M] (Lib.Mnemonics.pred M);
                        Lib.Mnemonics.assign [D] (Lib.Mnemonics.identity M);
                ] @
                Segment.deref seg @
                many (offset - 1) (Lib.Mnemonics.assign [A] (Lib.Mnemonics.succ A)) @
                [Lib.Mnemonics.assign [M] (Lib.Mnemonics.identity D)]
        else
                assign_i [D] offset @
                [
                        Segment.goto seg;
                        Lib.Mnemonics.assign [D] (Lib.Mnemonics.add M);
                        AInst "5"; (* TODO!!!   RAM[TEMP0]; *)
                        Lib.Mnemonics.assign [M] (Lib.Mnemonics.identity D);
                        SP.goto;
                        Lib.Mnemonics.assign [A; M] (Lib.Mnemonics.pred M);
                        Lib.Mnemonics.assign [D] (Lib.Mnemonics.identity M);
                        AInst "5";
                        Lib.Mnemonics.assign [A] (Lib.Mnemonics.identity M);
                        Lib.Mnemonics.assign [M] (Lib.Mnemonics.identity D);
                ]
