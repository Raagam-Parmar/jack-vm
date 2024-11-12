{
    open Parser
    open Vmast

    let reduce_segment (seg : string) : Segment.t = 
        match seg with
        | "argument" -> Segment.Argument
        | "local"    -> Segment.Local
        | "static"   -> Segment.Static
        | "constant" -> Segment.Constant
        | "this"     -> Segment.This
        | "that"     -> Segment.That
        | "pointer"  -> Segment.Pointer
        | "temp"     -> Segment.Temp
        | _          -> failwith "Lexer -> reduce_segment : Unexpected argument"
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

let segment      = "argument" | "local" | "static" | "constant" | "this" | "that" | "pointer" | "temp"

rule read = 
    parse
    | eof     { EOF  }

    | "push"  { PUSH }
    | "pop"   { POP  }
    | segment { SEGMENT (reduce_segment (Lexing.lexeme lexbuf)) }

    | "add"  { ADD }
    | "sub"  { SUB }
    | "neg"  { NEG }
    
    | "and"  { AND }
    | "or"   { OR  }
    | "not"  { NOT }

    | "gt"   { GT  }
    | "lt"   { LT  }
    | "eq"   { EQ  }

    | "label"  { LABEL  }
    | "goto"   { GOTO   }
    | "if-goto" { IFGOTO }

    | "function"  { FUNCTION }
    | "call"      { CALL     }
    | "return"    { RETURN   }

    | int           { INT    ( int_of_string (Lexing.lexeme lexbuf) ) }
    | allow_string  { STRING ( Lexing.lexeme lexbuf )                 }

    | ignore  { read lexbuf             }
    | "/*"    { skipMultiComment lexbuf }
    | "//"    { skipComment lexbuf      }

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
