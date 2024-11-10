open Jackvm

(** [parse s] parses [s] into an AST *)
let parse (s : string) : (string, string) Vmast.Program.t = 
        let lexbuf = Lexing.from_string s in
        let ast = Parser.main Lexer.read lexbuf in
        ast