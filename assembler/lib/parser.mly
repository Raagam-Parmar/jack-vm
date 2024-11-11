%token EOF

%token ZERO
%token ONE
%token BNOT
%token BOR
%token BAND
%token MINUS
%token PLUS
%token EQUAL
%token SEMI
%token A
%token D
%token M

%token <string>   LABEL
%token <string>   AINST
%token <int>      REF
%token <Ast.jump> JUMP

%nonassoc reg
%nonassoc A D M
%right    MINUS

%start <string Ast.program> main

%%

main:
        | EOF { [] }
        | program; EOF { $1 }
        ;

program:
        | instruction          { [$1] }
        | instruction; program { $1 :: $2 }
        ;

instruction:
        | a_instruction { Ast.AInst $1 }
        | c_instruction { Ast.CInst $1 }
        | ref           { Ast.Ref $1 }
        | LABEL         { Ast.Label $1 }
        ;

a_instruction:
        | AINST { $1 }
        ;

c_instruction:
        | dest; comp; jump { (Some $1, $2, Some $3) }
        | dest; comp { (Some $1, $2, None) }
        | comp; jump { (None, $1, Some $2) }
        | comp { (None, $1, None) }
        ;

dest:
        | register_list; EQUAL { $1 }
        ;

register_list: 
        | register { [$1] } 
        | register; register_list { $1 :: $2 }
        ;

%inline register:
        | A { Ast.A }
        | D { Ast.D }
        | M { Ast.M }
        ;

comp:
        | const                 { Ast.Constant $1 }
        | BNOT register         { Ast.Unary (BitNot, $2) }
        | MINUS register        { Ast.Unary (Minus, $2) }
        | register PLUS ONE     { Ast.Unary (Succ, $1) }
        | register MINUS ONE    { Ast.Unary (Pred, $1) }
        | D binary_op A         { Ast.Binary ($2, A) }
        | D binary_op M         { Ast.Binary ($2, M) }
        | A MINUS D             { Ast.Binary (SubFrom, A) }
        | M MINUS D             { Ast.Binary (SubFrom, M) }
        | register              { Ast.Unary (Identity, $1) }    %prec reg
        ;

%inline binary_op:
        | PLUS                  { Ast.Add }
        | MINUS                 { Ast.Sub }
        | BAND                  { Ast.BinAnd }
        | BOR                   { Ast.BinOr }

const:
        | ZERO                  { Ast.Zero }
        | ONE                   { Ast.One }
        | MINUS ONE             { Ast.MinusOne }
        ;

jump:
        | SEMI; JUMP { $2 }
        ;

ref:
        | REF { $1 }