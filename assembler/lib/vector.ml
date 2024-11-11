exception NegativeLength of int

let add (a : int list) (b : int list) : int list = 
        List.map2 ( + ) a b

let rec binary (n : int) : int list = 
        if (n < 0) then
                raise (NegativeLength n)
        else if n = 0 then
                [0]
        else
                if n / 2 = 0 then
                        [n mod 2]
                else
                        binary (n / 2) @ [n mod 2]

let rec fill_truncate (l : int) (v : int list) : int list = 
        if l < 0 then 
                raise (NegativeLength l)
        else
                let length = List.length v in
                if length = l then 
                        v
                else if l > length then
                        fill_truncate l (0 :: v)
                else
                        match v with
                        | [] -> []
                        | _ :: t -> t


let int (a : int list) : int = 
        List.fold_left (fun acc bit -> acc * 10 + bit) 0 a

let rec string (l : int list) : string = 
        match l with
        | [] -> ""
        | head :: tail -> (string_of_int head) ^ (string tail)
