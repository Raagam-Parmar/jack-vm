(** [trim_left rprefix:string s] removes all leading characters present in [rprefix] from [s]. *)
val trim_left : rprefix:string -> string -> string

(** [trim_right rsuffix:string s] removes all trailing characters present in [rsuffix] from [s]. *)
val trim_right : rsuffix:string -> string -> string

(** [trim rprefix:string  rsuffix:string s] removes all leading and trailing characters present
 in [rprefix] and [rsuffix], from [s], respectively. *)
val trim : rprefix:string -> rsuffix:string -> string -> string
