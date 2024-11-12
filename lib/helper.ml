open Lib

exception UnexpectedRepeat of int


let rec many (repeat : int) (elem : 'a) : 'a list = 
    if repeat < 0 then
        raise (UnexpectedRepeat repeat)
    else if repeat = 0 then
        []
    else
        elem :: (many (repeat - 1) elem)

let many_more (repeat : int) (elem_list : 'a list) : 'a list = 
    List.flatten (many repeat elem_list)
    
let assign_i (dest : Ast.destination) (i : int) : string Ast.instruction list = [
    AInst (string_of_int i);
    Mnemonics.assign dest (Mnemonics.identity A);
]

let copy (d : Ast.destination) (r : Ast.register) : string Ast.instruction = 
    Mnemonics.assign d (Mnemonics.identity r)
