module Segment = struct
    type t = 
    | Argument
    | Local
    | Static
    | Constant
    | This
    | That
    | Pointer
    | Temp
end

module Instruction = struct
    type offset = int
    type nArgs = int

    type ('f, 'l) t = 
    (* These are stack operations, push/pop segment n *)
    | Push of (Segment.t * offset)
    | Pop of (Segment.t * offset)

    (* Arithmetic operators *)
    | Add   | Sub   | Neg

    (* Comparison operators *)
    | Eq    | Gt    | Lt

    (* Bitwise operators *)
    | And   | Or    | Not

    (* These are Control flow instructions *)
    | Label of 'l
    | Goto of 'l
	| IfGoto of 'l

    (* These are function call and return instructions *)
    | Call of ('f * nArgs)
    | Ret

end

module Function = struct
    type ('f, 'l) t = {
        name: 'f;
        nVars: int;
        body: ('f, 'l) Instruction.t list
    }
end

module Program = struct
    type ('f, 'l) t = ('f, 'l) Function.t list
end
