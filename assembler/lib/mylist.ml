let rec mapi_offset ?(index=0) (f: int -> 'a -> 'b) (offset : int) (l : 'a list) = 
        match l with
        | [] -> []
        | h :: t -> (f (offset+index) h) :: (mapi_offset ~index:(index + 1) f offset t)
        