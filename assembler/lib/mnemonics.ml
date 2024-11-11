(** [jumpOf j] encodes jump instruction [j] into standard ASM instructions. *)
let jumpOf (j : Ast.jump) : string Ast.instruction = 
        match j with
        | Ast.JMP -> Ast.CInst (None, Constant (Zero), Some JMP)
        | _       -> Ast.CInst (None, Unary (Identity, D), Some j)

        
(** [jgt] is same as [jumpOf JGT]. *)
let jgt = jumpOf Ast.JGT

(** [jeq] is same as [jumpOf JEQ]. *)
let jeq = jumpOf Ast.JEQ

(** [jge] is same as [jumpOf JGE]. *)
let jge = jumpOf Ast.JGE

(** [jlt] is same as [jumpOf JLT]. *)
let jlt = jumpOf Ast.JLT

(** [jne] is same as [jumpOf JNE]. *)
let jne = jumpOf Ast.JNE

(** [jle] is same as [jumpOf JLE]. *)
let jle = jumpOf Ast.JLE

(** [jmp] is same as [jumpOf JMP]. *)
let jmp = jumpOf Ast.JMP


(** [add r] encodes into computation instruction [D + r]. *)
let add (r : Ast.register) = Ast.Binary (Add, r)

(** [sub r] encodes into computation instruction [D - r]. *)
let sub (r : Ast.register) = Ast.Binary (Sub, r)

(** [subFrom r] encodes into computation instruction [r - D]. *)
let subFrom (r : Ast.register) = Ast.Binary (SubFrom, r)

(** [binAnd r] encodes into computation instruction [D & r]. *)
let binAnd (r : Ast.register) = Ast.Binary (BinAnd, r)

(** [binOr r] encodes into computation instruction [D | r]. *)
let binOr (r : Ast.register) = Ast.Binary (BinOr, r)


(** [identity r] encodes into computation instruction [r]. *)
let identity (r : Ast.register) = Ast.Unary (Identity, r)

(** [bitNot r] encodes into computation instruction [! r]. *)
let bitNot (r : Ast.register) = Ast.Unary (BitNot, r)

(** [minus r] encodes into computation instruction [- r]. *)
let minus (r : Ast.register) = Ast.Unary (Minus, r)

(** [succ r] encodes into computation instruction [r + 1]. *)
let succ (r : Ast.register) = Ast.Unary (Succ, r)

(** [pred r] encodes into computation instruction [r - 1]. *)
let pred (r : Ast.register) = Ast.Unary (Pred, r)


(** [assign d c] assigns register destination [d] the value of computation [c], without any jump instruction. *)
let assign (d : Ast.destination) (c : Ast.computation) : string Ast.instruction = 
        CInst (Some d, c, None)