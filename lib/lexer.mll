{
        open Parser
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

rule read = 
        parse
        | eof           { EOF }