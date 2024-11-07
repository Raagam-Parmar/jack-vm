let add_vector (a : int list) (b : int list) : int list = 
        List.map2 ( + ) a b

let rec to_binary (n : int) : int list = 
        if (n < 0) then
                failwith "Vector.to_binary: expected positive integer, received negative integer"
        else if n = 0 then
                [0]
        else
                if n / 2 = 0 then
                        [n mod 2]
                else
                        to_binary (n / 2) @ [n mod 2]

let rec fill_truncate (l : int) (v : int list) : int list = 
        if l < 0 then 
                failwith "Vector.fill_truncate : expected positive length, received negative length"
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


let to_int (a : int list) : int = 
        List.fold_left (fun acc bit -> acc * 10 + bit) 0 a

let rec to_string (l : int list) : string = 
        match l with
        | [] -> ""
        | head :: tail -> (string_of_int head) ^ (to_string tail)
              

(* let to_int (l : int list) : int =  *)
                

(* let ( ++ ) (a : 'a list) (b : 'a list) : 'a list = a @ b  *)