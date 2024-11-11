(** [add v1 v2] component-wise adds two vectors [v1] and [v2] of same length. *)
val add : int list -> int list -> int list

(** [binary i] converts [i] into its vector binary representation. It raises [NegativeLength] if [i] is negative. *)
val binary : int -> int list

(** [fill_truncate l v] prepends [v] with zeroes if lengh of [v] is less than [l].
        Otherwise, it truncates [v] to length [l]. It raises [NegativeLength] if [i] is negative. *)
val fill_truncate : int -> int list -> int list

(** [int v] converts binary vector [v] into its decimal representation. *)
val int : int list -> int

(** [string v] convets binary vector [v] into its binary string representation. *)
val string : int list -> string