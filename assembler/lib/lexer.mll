{
        open Parser

        exception UnexpectedArgument of string

        (** [reduce_a_inst a] reduces [a] recognised by the RegEx into string without leading ["@"] or [" "].  *)
        let reduce_a_inst (a : string) : string = 
                String_tools.trim_left ~rprefix:"@ " a

        (** [reduce_label l] reduces [l] recognised by RegEx into a string by removing leading an trailing ["("], [")"] and [" "]. *)
        let reduce_label (l : string) : string = 
                String_tools.trim ~rprefix:"( " ~rsuffix:" )" l

        (** [reduce_jump j] reduces [j] recognised by RegEx into [Ast.jump] type. It raises [UnexpectedArgument]
        if it is called on a string which is not matched by the RegEx. *)
        let reduce_jump (j : string) : Ast.jump = 
                match j with
                | "JGT" -> JGT
                | "JEQ" -> JEQ
                | "JGE" -> JGE
                | "JLT" -> JLT
                | "JNE" -> JNE
                | "JLE" -> JLE
                | "JMP" -> JMP
                | _     -> raise (UnexpectedArgument j)
        
        (** [reduce_refF rf] reduces [rf] recognised by RegEx, which is of the form ["^+i"] and returns [i] integer. *)
        let reduce_refF (rf : string) : int = 
                int_of_string (String_tools.trim_left ~rprefix:"@^+ " rf)
        
        (** [reduce_refB rf] reduces [rf] recognised by RegEx, which is of the form ["^-i"] and returns [- i] integer. *)
        let reduce_refB (rb : string) : int = 
                -1 * int_of_string (String_tools.trim_left ~rprefix:"@^- " rb)
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

let label        = '(' (allow_string) ')'

let ainst        = '@' (spaces*) (allow_string) | '@' (spaces*) (int)
let refF         = '@' (spaces*) '^' (spaces*) '+' (spaces*) (int)
let refB         = '@' (spaces*) '^' (spaces*) '-' (spaces*) (int)



rule read = 
        parse
        | eof           { EOF   }
        | '0'           { ZERO  }
        | '1'           { ONE   }
        | '!'           { BNOT  }
        | '|'           { BOR   }
        | '&'           { BAND  }
        | '-'           { MINUS }
        | '+'           { PLUS  }
        | '='           { EQUAL }
        | ';'           { SEMI  }
        | 'A'           { A }
        | 'D'           { D }
        | 'M'           { M }

        | label         { LABEL (reduce_label (Lexing.lexeme lexbuf))  }
        | jump          { JUMP (reduce_jump (Lexing.lexeme lexbuf))    }
        | ainst         { AINST (reduce_a_inst (Lexing.lexeme lexbuf)) }
        | refF          { REF (reduce_refF (Lexing.lexeme lexbuf))     }
        | refB          { REF (reduce_refB (Lexing.lexeme lexbuf))     }

        | ignore        { read lexbuf             }
        | "/*"          { skipMultiComment lexbuf }
        | "//"          { skipComment lexbuf      }

and skipComment = 
        parse
        | [^'\n']       { skipComment lexbuf }
        | eof           { EOF }
        | _             { read lexbuf }
        
and skipMultiComment = 
        parse
        | [^'*']        { skipMultiComment lexbuf }
        | '*'           { endMultiComment lexbuf  }
        | eof           { EOF }

and endMultiComment =
        parse
        | '/'           { read lexbuf }
        | eof           { EOF }
        | _             { skipMultiComment lexbuf }