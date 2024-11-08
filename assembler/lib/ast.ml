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

type 'l label = Label of 'l

module Block : sig
        type 'v t = 'v label option * 'v instruction list
        val label_opt : 'v t -> 'v label option
        val label : 'v t -> 'v label
        val has_label : 'v t -> bool
        val instructions : 'v t -> 'v instruction list
        val length : 'v t -> int
end = struct
        type 'v t = 'v label option * 'v instruction list

        let label_opt (b : 'v t) : 'v label option = 
                fst b

        let label (b : 'v t) : 'v label = 
                match fst b with
                | Some l -> l
                | None   -> failwith "Ast.Block.get_label: no label defined"
        
        let has_label (b : 'v t) : bool = 
                match fst b with
                | Some _ -> true
                | None   -> false

        let instructions (b : 'v t) : 'v instruction list = 
                snd b

        let length (b : 'v t) : int = 
                List.length (instructions b)
(*         
        let rec cinst_count (b : 'v t) : int = 
                match instructions b with
                | []      -> 0
                | CInst _ :: tail -> 1 + (cinst_count (None, tail))
                | AInst _ :: tail -> cinst_count (None, tail)
        
        let rec ainst_count (b : 'v t) : int = 
                match instructions b with
                | []      -> 0
                | CInst _ :: tail -> ainst_count (None, tail)
                | AInst _ :: tail -> 1 + (ainst_count (None, tail)) *)

        (* let rec symbols (b : 'v t) : 'v list = 
                let insts = instructions b in
                match insts with
                | [] -> []
                | CInst _ :: tail -> symbols (None, tail)
                | AInst v :: tail -> v :: symbols (None, tail) *)
end

type 'v program = 'v Block.t list

(*
type ('v, 'l) block = 'l label option * 'v instruction list

type ('v, 'l) program = ('v, 'l) block list
*)