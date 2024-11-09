%token EOF                      // End Of File token

%start <(string, string) Vmast.Function.t list> main

%%

main:
    | EOF { [] } 