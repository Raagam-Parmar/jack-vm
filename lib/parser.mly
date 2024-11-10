%{
    open Vmast
%}

%token EOF

%token <string> STRING
%token <int>    INT

%token PUSH
%token POP
%token <Vmast.Segment.t> SEGMENT

%token ADD
%token SUB
%token NEG

%token AND
%token OR
%token NOT

%token GT
%token LT
%token EQ

%token LABEL
%token GOTO
%token IFGOTO

%token FUNCTION
%token CALL
%token RETURN

%start <(string, string) Function.t list> main

%%

main:
    | EOF         { [] }
    | program EOF { $1 }
    ;

program:
    | funct         {  [ $1 ]  }
    | funct program { $1 :: $2 }
    ;

funct:
    | FUNCTION STRING INT instruction_list { Function.{name = $2; nVars = $3; body = $4} }
    ;

instruction_list:
    | instruction                  {  [ $1 ]  }
    | instruction instruction_list { $1 :: $2 }
    ;

instruction:
    | PUSH SEGMENT INT { Instruction.Push ($2, $3) }
    | POP  SEGMENT INT { Instruction.Pop  ($2, $3) }

    | ADD { Instruction.Add }
    | SUB { Instruction.Sub }
    | NEG { Instruction.Neg }

    | AND { Instruction.And }
    | OR  { Instruction.Or  }
    | NOT { Instruction.Not }

    | EQ  { Instruction.Eq  }
    | GT  { Instruction.Gt  }
    | LT  { Instruction.Lt  }

    | LABEL  STRING { Instruction.Label  $2 }
    | GOTO   STRING { Instruction.Goto   $2 }
    | IFGOTO STRING { Instruction.IfGoto $2 }

    | CALL   STRING INT { Instruction.Call ($2, $3) }
    | RETURN            { Instruction.Ret }
    ;
