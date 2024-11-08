let jumpOf (j : Ast.jump) : string Ast.instruction = 
        match j with
        | Ast.JMP -> Ast.CInst (None, Constant (Zero), Some JMP)
        | _       -> Ast.CInst (None, Unary (Identity, D), Some j)

let jgt = jumpOf Ast.JGT
let jeq = jumpOf Ast.JEQ
let jge = jumpOf Ast.JGE
let jlt = jumpOf Ast.JLT
let jne = jumpOf Ast.JNE
let jle = jumpOf Ast.JLE
let jmp = jumpOf Ast.JMP

let add (r : Ast.register) = Ast.Binary (Add, r)
let sub (r : Ast.register) = Ast.Binary (Sub, r)
let subFrom (r : Ast.register) = Ast.Binary (SubFrom, r)
let binAnd (r : Ast.register) = Ast.Binary (BinAnd, r)
let binOr (r : Ast.register) = Ast.Binary (BinOr, r)

let identity (r : Ast.register) = Ast.Unary (Identity, r)
let bitNot (r : Ast.register) = Ast.Unary (BitNot, r)
let minus (r : Ast.register) = Ast.Unary (Minus, r)
let succ (r : Ast.register) = Ast.Unary (Succ, r)
let pred (r : Ast.register) = Ast.Unary (Pred, r)

let assign (d : Ast.destination) (c : Ast.computation) : string Ast.instruction = 
        CInst (Some d, c, None)