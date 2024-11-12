open Lib

(** [goto] produces ASM instruction which translates to [@SP] *)
val goto : string Ast.instruction

(** [deref] produces ASM instructions which translate to [@SP; A=M], effectively making A-register point to 
the memory location given in [SP] *)
val deref : string Ast.instruction list

(** [deref_sub] is similar to [deref], except it decerases [SP] and points to one memory-address below/lesser than [SP]*)
val deref_sub : string Ast.instruction list

(** [incr] increments [SP] by one *)
val incr : string Ast.instruction list

(** [decr] decrements [SP] by one *)
val decr : string Ast.instruction list

(** [assignD] assigns the value in D-register to [*SP] (dereferenced SP) *)
val assignD : string Ast.instruction list