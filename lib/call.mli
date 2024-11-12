open Lib

(** [v func n addr] calls function [f] with [n] arguments using [addr] as the unique return address. *)
val v : string -> Vmast.Instruction.nArgs -> string -> string Ast.instruction list

(** [bootstrap] is the shortened call for bootstrapping (calling ["Sys.init"]). *)
val bootstrap : string Ast.instruction list

(** [save addr n] generated ASM instructions to save the frame of the calling-function, which is calling 
a function with [n] arguments. *)
val save : string -> Vmast.Instruction.nArgs -> string Ast.instruction list