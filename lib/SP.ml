open Lib

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