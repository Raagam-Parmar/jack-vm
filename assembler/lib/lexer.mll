{
        open Parser
        (* open Lexing *)
(* 
        (** [next_line lexbuf] updates the position of the current lexical buffer [lexbuf]
        to point towards the start of the next line *)
        let next_line (lexbuf : lexbuf) =
                let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <- 
                {
                        pos with pos_bol = pos.pos_cnum;
                        pos_lnum = pos.pos_lnum + 1
                } *)

        let reduce_a_inst (a : string) : string = 
                String_tools.trim_left ~rprefix:"@ " a

        let reduce_label (l : string) : string = 
                String_tools.trim ~rprefix:"( " ~rsuffix:" )" l

        let reduce_jump (j : string) : Ast.jump = 
                match j with
                | "JGT" -> JGT
                | "JEQ" -> JEQ
                | "JGE" -> JGE
                | "JLT" -> JLT
                | "JNE" -> JNE
                | "JLE" -> JLE
                | "JMP" -> JMP
                | _     -> failwith "Lexer.reduce_jump : Unexpected Argument"

}

let space        = [' ' '\t']
let spaces       = space+
let newline      = '\r' | '\n' | "\r\n"

let ignore       = space | newline

let digit        = ['0'-'9']
let int          = digit+

let lower_char   = ['a'-'z']
let upper_char   = ['A'-'Z']
let allowed_misc = ':' | '.' |'$' | '_'
let allow_first  = lower_char | upper_char | allowed_misc
let allow_rest   = lower_char | upper_char | allowed_misc | digit

let allow_string = (allow_first) (allow_rest)*

let jump         = "JGT" | "JGE" | "JEQ" | "JNE" | "JLE" | "JLT" | "JMP"

(* let register     = 'A' | 'D' | 'M' *)

let label        = '(' (allow_string) ')'

let ainst        = '@' (spaces*) (allow_string) | '@' (spaces*) (int)


rule read = 
        parse
        | eof           { EOF }
        | '0'           { ZERO }
        | '1'           { ONE }
        | '!'           { BNOT }
        | '|'           { BOR }
        | '&'           { BAND }
        | '-'           { MINUS }
        | '+'           { PLUS }
        | '='           { EQUAL }
        | ';'           { SEMI }
        | 'A'           { A }
        | 'D'           { D }
        | 'M'           { M }

        | label         { LABEL (reduce_label (Lexing.lexeme lexbuf)) }
        | jump          { JUMP (reduce_jump (Lexing.lexeme lexbuf)) }
        | ainst         { AINST (reduce_a_inst (Lexing.lexeme lexbuf)) }

        | ignore        { read lexbuf }
        | "/*"          { skipMultiComment lexbuf }
        | "//"          { skipComment lexbuf }

and skipComment = 
        parse
        | [^'\n']       { skipComment lexbuf }
        | eof           { EOF }
        | _             { read lexbuf }
        
and skipMultiComment = 
        parse
        | [^'*']        { skipMultiComment lexbuf }
        | '*'           { endMultiComment lexbuf }
        | eof           { EOF }

and endMultiComment =
        parse
        | '/'           { read lexbuf }
        | eof           { EOF }
        | _             { skipMultiComment lexbuf }