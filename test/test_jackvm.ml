open Jackvm

let a = Snippets.Segment.push Pointer 1 "Foo"



let b : string Lib.Ast.program = [(None, a)]
let h = Hashtbl.create 10
let () = Lib.Machine.LabelTable.populate h b
let () = Lib.Machine.VarTable.populate h b

let c = Lib.Machine.Program.encode_pretty_string h b

let () = List.iter print_endline c