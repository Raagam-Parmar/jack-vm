(** [generate ()] generates a unique return address every time it is called. It is of the form
["RET_ADDRESS_CALLi"] where integer [i] is unique every time this function is called. *)
val generate : unit -> string

(** [bootstrap] is the string which is used for the Bootstrap code under [Bootstrap.v] *)
val bootstrap : string