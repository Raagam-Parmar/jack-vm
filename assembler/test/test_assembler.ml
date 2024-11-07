(* open OUnit2 *)
open Lib

(*
let make_test name value (ast : string Ast.program) = 
        name >:: (fun _ -> assert_equal value (Lib.Machine.Program.encode_pretty_string ) ) *)

(* let b1 : string Ast.Block.t = (Some (Label "START"), [])
let b2 : string Ast.Block.t = (Some (Label "FOO"), [AInst "2"; CInst (Some [D],Unary (Identity, A), None)])
let b3 : string Ast.Block.t = ( Some (Label "BAR"), [ AInst "3"; CInst (Some [D], Binary (Add, A), None); AInst "0"; CInst (Some [M], Unary (Identity, M), None) ] )

let p : string Ast.program = [b1; b2; b3] *)

let a = [(Some (Lib.Ast.Label "START"), []);
(Some (Lib.Ast.Label "F00"),
 [Lib.Ast.AInst "2";
  Lib.Ast.CInst
   (Some [Lib.Ast.D], Lib.Ast.Unary (Lib.Ast.Identity, Lib.Ast.A), None)]);
(Some (Lib.Ast.Label "BAR"),
 [Lib.Ast.AInst "3";
  Lib.Ast.CInst
   (Some [Lib.Ast.D], Lib.Ast.Binary (Lib.Ast.Add, Lib.Ast.A), None);
  Lib.Ast.AInst "0";
  Lib.Ast.CInst
   (Some [Lib.Ast.M], Lib.Ast.Unary (Lib.Ast.Identity, Lib.Ast.D), None)])]

let h = Hashtbl.create 10
let () = Machine.Program.populate h a

let sl = Machine.Program.encode_pretty_string h a
let () = List.iter print_endline sl