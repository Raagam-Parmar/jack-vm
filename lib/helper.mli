open Lib


(** [many r e] repeats [e] in a list [r] number of times, raising [UnexpectedRepeat r] if [r] is negative *)
    val many : int -> 'a -> 'a list

(** [many_more r el] repeats list [el] in a flattened list [r] number of times, raising [UnexpectedRepeat r] if [r] is negative *)
val many_more : int -> 'a list -> 'a list

(** [asssign_i dest i] assigns the integer value [i] to destination(s) [dest] *)
val assign_i : Ast.destination -> int -> string Ast.instruction list

(** [copy dest r] copies the memory value in register [r] and assigns it to destination(s) [dest] *)
val copy : Ast.destination -> Ast.register -> string Ast.instruction