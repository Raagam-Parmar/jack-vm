open Lib

(** [goto seg] generates ASM instructions which translate to [@seg].
For instance, [goto Local] will translate into [@LCL].
It raises [UnexpectedSegment seg] if [seg] is not
    1. Argument
    2. Local
    3. This
    4. That 
    *)
val goto : Vmast.Segment.t -> string Ast.instruction list

(** [deref seg] generates ASM instructions which translate to [@seg; A=M].
They effectively point to the memory address stored in [seg] 
It raises [UnexpectedSegment seg] if [seg] is not
    1. Argument
    2. Local
    3. This
    4. That *)
val deref : Vmast.Segment.t -> string Ast.instruction list

(** [push seg n] pushes onto the VM stack, the value at offset [n] from the base-pointer of [seg] *)
val push : Vmast.Segment.t -> int -> string -> string Ast.instruction list

(** [pop seg n] pops from the VM stack, into offset [n] from the base-pointer of [seg].
It raises [PopToConstant] if VM tries to pop a value into the [Constant] segment. *)
val pop : Vmast.Segment.t -> int -> string -> string Ast.instruction list