type jump = 
        | JGT
        | JEQ
        | JGE
        | JLT
        | JNE
        | JLE
        | JMP

type register = A | D | M

type destination = register list

type const = 
        | Zero
        | One
        | MinusOne

type unOp = 
        | Identity
        | BitNot
        | Minus
        | Succ
        | Pred

type biOp = 
        | Add
        | Sub
        | SubFrom
        | BinAnd
        | BinOr

type computation = 
        | Constant of const
        | Unary of unOp * register
        | Binary of biOp * register
        (** the binary operations are applied to D, register which is implied *)

type c_inst = destination option * computation * jump option 

type 'v instruction = 
        | AInst of 'v
        | CInst of c_inst
        | Ref of int
        | Label of 'v

type 'v program = 'v instruction list
