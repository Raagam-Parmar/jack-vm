exception UnexpectedRegister of string
exception UnexpectedStrArgument of string Ast.instruction
exception UnexpectedIntArgument of int Ast.instruction
exception MissingSymbol of string
exception RepeatedLabel of string

module Jump : sig
        (** [encode j] encodes [j] into a 3-vector [jjj]. *)
        val encode : Ast.jump option -> int list
end = struct
        let encode_j (j : Ast.jump) : int list = 
                match j with
                | JGT -> [0; 0; 1]
                | JEQ -> [0; 1; 0]
                | JGE -> [0; 1; 1]
                | JLT -> [1; 0; 0]
                | JNE -> [1; 0; 1]
                | JLE -> [1; 1; 0]
                | JMP -> [1; 1; 1]

        let encode (jump : Ast.jump option) : int list = 
                match jump with
                | Some j -> encode_j j
                | None   -> [0; 0; 0]
end


module Dest : sig
        (** [encode d] encodes [d] into a 3-vector [ddd]. *)
        val encode : Ast.destination option -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | M -> [0; 0; 1]
                | D -> [0; 1; 0]
                | A -> [1; 0; 0]
        
        let encode_d (d : Ast.destination) : int list = 
                let vecM = if List.mem Ast.M d then encode_r Ast.M else [0; 0; 0] in
                let vecD = if List.mem Ast.D d then encode_r Ast.D else [0; 0; 0] in
                let vecA = if List.mem Ast.A d then encode_r Ast.A else [0; 0; 0] in
                Vector.add (Vector.add vecM vecD) vecA

        let encode (dest : Ast.destination option) : int list = 
                match dest with
                | Some d -> encode_d d
                | None   -> [0; 0; 0]     
end


module Const : sig
        (** [encode c] encodes constant (0, 1, -1) [c] into a 7-vector [ccccccc]. *)
        val encode : Ast.const -> int list
end = struct
        let encode (c : Ast.const) : int list = 
                match c with
                | Zero      -> [0; 1; 0; 1; 0; 1; 0]
                | One       -> [0; 1; 1; 1; 1; 1; 1]
                | MinusOne  -> [0; 1; 1; 1; 0; 1; 0]
end


module Unary : sig
        (** [encode uop r] encodes encodes binary instructions of applying unary [uop] on register [r]; into a 7-vector [ccccccc]. *)
        val encode : Ast.unOp -> Ast.register -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | D -> [0; 0; 0; 1; 1]
                | A -> [0; 1; 1; 0; 0]
                | M -> [1; 1; 1; 0; 0]
        let encode_op (uop : Ast.unOp) : int list = 
                match uop with
                | Identity -> [0; 0]
                | BitNot   -> [0; 1]
                | Minus    -> [1; 1]
                | Succ     -> [1; 1]
                | Pred     -> [1; 0]
        
        let succ (r : Ast.register) : int list =
                match r with
                | D -> [0; 0; 1; 1; 1; 1; 1]
                | A -> [0; 1; 1; 0; 1; 1; 1]
                | M -> [1; 1; 1; 0; 1; 1; 1]
        
        let encode (uop : Ast.unOp) (r : Ast.register) : int list = 
                match uop with
                | Ast.Succ      -> succ r
                | _             -> encode_r r @ encode_op uop       
end


module Binary : sig
        (** [encode bop r] encodes encodes binary instructions of applying binary [bop] on register [r]; into a 7-vector [ccccccc]. *)
        val encode : Ast.biOp -> Ast.register -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | Ast.A -> [0]
                | Ast.M -> [1]
                | Ast.D -> raise (UnexpectedRegister "Can not apply binary operations on register D")
        let encode_op (bop : Ast.biOp) : int list = 
                match bop with
                | Add     -> [0; 0; 0; 0; 1; 0]
                | Sub     -> [0; 1; 0; 0; 1; 1]
                | SubFrom -> [0; 0; 0; 1; 1; 1]
                | BinAnd  -> [0; 0; 0; 0; 0; 0]
                | BinOr   -> [0; 1; 0; 1; 0; 1]

        let encode (bop : Ast.biOp) (r : Ast.register)  : int list = 
                encode_r r @ encode_op bop
end


module Comp : sig
        (** [encode comp] uses [Binary.encode], [Unary.encode], [Dest.encode] and [Jump.encode] to encode [comp] into a 13-vector [cccccccdddjjj]. *)
        val encode : Ast.computation -> int list
end = struct
        let encode (comp : Ast.computation) : int list = 
                match comp with
                | Ast.Constant c -> Const.encode c
                | Ast.Unary (u, r) -> Unary.encode u r
                | Ast.Binary (b, r) -> Binary.encode b r
end


module Cinst : sig
        (** [encode cinst] uses [Comp.encode] to encode [cinst] into a 16-vector [111cccccccdddjjj]. *)
        val encode : Ast.c_inst -> int list
end = struct
        let encode (c : Ast.c_inst) : int list = 
                match c with (dest, comp, jump) -> [1; 1; 1] @ (Comp.encode comp) @ (Dest.encode dest) @ (Jump.encode jump)

end


module Ainst : sig
        (** [convert h i] looks up binding of [i] in Hashtable [h] and converts [i : string Ast.instruction] into [int Ast.instruction]. *)
        val convert : (string, int) Hashtbl.t -> string Ast.instruction -> int Ast.instruction

        (** [encode i] encodes [i] into a 16-vector [1<truncated binary representation of i>]. *)
        val encode : int Ast.instruction -> int list
end = struct
        (** [resolve_symbol h s] checks if [s] is a number and returns it as is, if [s] is a string, then checks binding of [s] in Hashtable [h]. 
                It raises [MissingSymbol] if [s : string] doesn't have a binding in [h]. *)
        let resolve_symbol (h : (string, int) Hashtbl.t) (symbol : string) : int = 
                match int_of_string_opt symbol with
                | Some x -> x
                | None   -> 
                        if Hashtbl.mem h symbol then
                                Hashtbl.find h symbol
                        else 
                                raise (MissingSymbol symbol)
                
        let convert (h : (string, int) Hashtbl.t) (a : string Ast.instruction) : int Ast.instruction = 
                match a with
                | AInst s -> AInst (resolve_symbol h s)
                | CInst _ -> raise (UnexpectedStrArgument a)
                | Ref _   -> raise (UnexpectedStrArgument a)
                | Label _ -> raise (UnexpectedStrArgument a)
        
        let encode (a : int Ast.instruction) : int list = 
                match a with
                | AInst i -> Vector.fill_truncate 16 (Vector.binary i)
                | CInst _ -> raise (UnexpectedIntArgument a)
                | Ref _   -> raise (UnexpectedIntArgument a)
                | Label _ -> raise (UnexpectedIntArgument a)
end


module Skip : sig
        (** [convert i] converts [i] into an [int Ast.instruction]. *)
        val convert : int -> int Ast.instruction
end = struct
        let convert (line_offset : int) : int Ast.instruction = 
                AInst line_offset
end

module Instruction : sig
        (** [encode h lo a] encodes [a] into 16-vector using [Ainst.encode] for Ainst, and [Skip.convert] if [a] is a reference.
                [lo] is the offset in the program at which [a] is present. *)
        val encode : (string, int) Hashtbl.t -> int -> string Ast.instruction ->  int list
end = struct
        let encode (h : (string, int) Hashtbl.t) (line_offset : int) (a : string Ast.instruction) : int list = 
                match a with
                | AInst _      -> Ainst.encode (Ainst.convert h a)
                | CInst c      -> Cinst.encode c
                | Ref i        -> Ainst.encode (Skip.convert (line_offset + i))
                | Label _      -> []
end


module LabelTable : sig
        (** [populate h p] populates Hashtable [h] with labels in program [p]. It raises  *)
        val populate : ?offset:int -> (string, int) Hashtbl.t -> string Ast.program -> unit
end = struct
        let rec populate ?(offset=0) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                let new_offset = offset + 1 in
                match p with
                | [] -> ()
                | Label l :: tail -> 
                        (
                        if Hashtbl.mem h l then 
                                raise (RepeatedLabel l)
                        else 
                                Hashtbl.add h l offset
                        ); populate ~offset:offset h tail
                | AInst _ :: tail -> populate ~offset:new_offset h tail
                | CInst _ :: tail -> populate ~offset:new_offset h tail
                | Ref _   :: tail -> populate ~offset:new_offset h tail
end


(** TODO: make this code better *)
module VarTable : sig
        (** [populate h p] populates Hashtable [h] with built-in symbol bindings and program variables in the program [p]. *)
        val populate : ?offset:int -> (string, int) Hashtbl.t -> string Ast.program -> unit
end = struct
        (** [register_symbol i] gives the built-in register symbol for 0 <= [i] <= 15, which is ["Ri"]. *)
        let register_symbol (i : int) : string = 
                String.cat "R" (string_of_int i)

        (** [init_registers start_index:int end_index:int h] populates Hashtable [h] with register symbols from [start_index] to [end_index]. *)
        let rec init_registers ?(start_index=0) ?(end_index=15) (h : (string, int) Hashtbl.t) : unit = 
                let new_start_index = start_index + 1 in
                if start_index <= end_index then 
                        (
                        Hashtbl.add h (register_symbol start_index) start_index;
                        init_registers h ~start_index:new_start_index ;
                        )

        (** [init_symbols h] populates Hashtable [h] with built-in symbols of Hack. *)
        let init_symbols (h : (string, int) Hashtbl.t) : unit = 
                (
                Hashtbl.add h  "SCREEN" 16384;
                Hashtbl.add h     "KBD" 24756;
                Hashtbl.add h      "SP" 0;
                Hashtbl.add h     "LCL" 1;
                Hashtbl.add h     "ARG" 2;
                Hashtbl.add h    "THIS" 3;
                Hashtbl.add h    "THAT" 4;
                )

        (** [init h] calls [init_symbols] and [init_registers] on Hashtable [h]. *)
        let init (h : (string, int) Hashtbl.t) : unit = 
                (
                init_registers h;
                init_symbols h;
                )

        (** [edit_program offset:int h p] populates Hashtable [h] with program variables and their binding addresses, 
        which begin from 16 ([offset]) by default. *)
        let rec edit_program ?(offset=16) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                match p with
                | [] -> ()
                | Label _ :: tail -> edit_program ~offset:offset h tail
                | CInst _ :: tail -> edit_program ~offset:offset h tail
                | Ref _   :: tail -> edit_program ~offset:offset h tail
                | AInst a :: tail -> 
                                match  int_of_string_opt a with
                                | Some _ -> edit_program ~offset:offset h tail
                                | None   -> 
                                        (
                                        if not (Hashtbl.mem h a) then
                                                (
                                                Hashtbl.add h a offset;
                                                let new_offset = offset + 1 in
                                                edit_program ~offset:new_offset h tail
                                                )
                                        else
                                                edit_program ~offset:offset h tail
                                        )

        let populate ?(offset=16) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                init h;
                edit_program ~offset:offset h p
end


module Program : sig
        (** [v p] returns the Hack assembly instructions for Assembly program [p]. *)
        val v : string Ast.program -> string list
        
        (** [encode h p] encodes program [p] into 16-vectors, each vector corresponding to an instruction (except for [Label]).
        It uses [h] to look up the bindings of Labels and Program Variables in [p]. *)
        val encode : ?offset:int -> (string, int) Hashtbl.t -> string Ast.program -> int list list

        (** [encode_pretty_string h p] is same as [encode h p] except it converts all 16-vectors into binary strings. *)
        val encode_pretty_string : (string, int) Hashtbl.t -> string Ast.program -> string list

        (** [populate h p] populates Hashtable [h] with labels and program variables in [p]. *)
        val populate : (string, int) Hashtbl.t -> string Ast.program -> unit
end = struct
        let populate (h : (string, int) Hashtbl.t) ( p : string Ast.program) : unit = 
                LabelTable.populate h p;
                VarTable.populate h p

        let rec encode ?(offset=0) (h : (string, int) Hashtbl.t) (p : string Ast.program) : int list list = 
                match p with
                | [] -> []
                | i :: is ->
                        match i with
                        | Label _ -> encode ~offset:(offset) h is 
                        | AInst _ -> Instruction.encode h offset i :: encode ~offset:(offset + 1) h is
                        | CInst _ -> Instruction.encode h offset i :: encode ~offset:(offset + 1) h is
                        | Ref _   -> Instruction.encode h offset i :: encode ~offset:(offset + 1) h is
        
        let encode_pretty_string (h : (string, int) Hashtbl.t) (p : string Ast.program) : string list = 
                List.map Vector.string (encode h p)
        
        let v (p : string Ast.program) : string list = 
                let h = Hashtbl.create 10 in
                let () = populate h p in
                encode_pretty_string h p
end