%token EOF                      // End Of File token

%token ZERO
%token ONE
// %token MINUS_ONE
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

%token <string> LABEL           // Label token carrying a string
%token <string> AINST           // AInst token carrying a string
%token <Ast.jump> JUMP            // Jump token carrying a string

%nonassoc reg
%nonassoc ZERO ONE BNOT MINUS AINST //  MINUS_ONE
%nonassoc A D M
%nonassoc inst_list
%nonassoc label

%start <string Ast.program> main

%%

main:
        | EOF { [] }
        | program; EOF { $1 }
        ;

program:
        | block { [$1] }
        | block; program { $1 :: $2 }
        ;

block:
        | LABEL; instruction_list { (Some (Ast.Label $1), $2) }
        | instruction_list { (None, $1) }
        | LABEL { (Some (Ast.Label $1), []) } %prec label
        ;

instruction_list:
        | instruction                   { [$1] }        %prec inst_list
        | instruction; instruction_list { $1 :: $2 }
        ;

instruction:
        | a_instruction { Ast.AInst $1 }
        | c_instruction { Ast.CInst $1 }
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

        // | BNOT A                { Ast.Unary (BitNot, A) }
        // | BNOT D                { Ast.Unary (BitNot, D) }
        // | BNOT M                { Ast.Unary (BitNot, M) }
        | BNOT register         { Ast.Unary (BitNot, $2) }

        // | MINUS A               { Ast.Unary (Minus, A) }
        // | MINUS D               { Ast.Unary (Minus, D) }
        // | MINUS M               { Ast.Unary (Minus, M) }
        | MINUS register        { Ast.Unary (Minus, $2) }

        // | A PLUS ONE            { Ast.Unary (Succ, A) }
        // | D PLUS ONE            { Ast.Unary (Succ, D) }
        // | M PLUS ONE            { Ast.Unary (Succ, M) }
        | register PLUS ONE     { Ast.Unary (Succ, $1) }

        // | A MINUS_ONE           { Ast.Unary (Pred, A) }
        // | D MINUS_ONE           { Ast.Unary (Pred, D) }
        // | M MINUS_ONE           { Ast.Unary (Pred, M) }
        | register MINUS ONE    { Ast.Unary (Pred, $1) }  // can i make this minus one?

        // | D PLUS A              { Ast.Binary (Add, A) }
        // | D MINUS A             { Ast.Binary (Sub, A) }
        // | D BAND A              { Ast.Binary (BinAnd, A) }
        // | D BOR A               { Ast.Binary (BinOr, A) }
        | D binary_op A         { Ast.Binary ($2, A) }

        // | D PLUS M              { Ast.Binary (Add, M) }
        // | D MINUS M             { Ast.Binary (Sub, M) }
        // | D BAND M              { Ast.Binary (BinAnd, M) }
        // | D BOR M               { Ast.Binary (BinOr, M) }
        | D binary_op M         { Ast.Binary ($2, M) }

        | A MINUS D             { Ast.Binary (SubFrom, A) }
        | M MINUS D             { Ast.Binary (SubFrom, M) }

        // | A                     { Ast.Unary (Identity, A) }  %prec reg
        // | D                     { Ast.Unary (Identity, D) }  %prec reg
        // | M                     { Ast.Unary (Identity, M) }  %prec reg
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