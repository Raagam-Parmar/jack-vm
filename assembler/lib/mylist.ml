(** [mapi_offset f offset l] is same as [List.mapi] but with an additional parameter of [offset] exposed to the function, 
which gets added to [index] before it is passed asn anargument to the function [f]. *)
let rec mapi_offset ?(index=0) (f: int -> 'a -> 'b) (offset : int) (l : 'a list) = 
        match l with
        | [] -> []
        | h :: t -> (f (offset + index) h) :: (mapi_offset ~index:(index + 1) f offset t)
        