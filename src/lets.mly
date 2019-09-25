%token LET
%token EQ
%token<string> UWORD
%token<string> WORD
%token EOF

%start<(string * string list * string list) list>  start
%%

start:
  | list(any) l=list(phrase) EOF { List.flatten l }

phrase:
  | LET WORD list(any_word) EQ body { [] }
  | LET v=UWORD arg=list(any_word) EQ def=body { [v,arg,def] }


body:
 | w=any b=body { w :: b }
 | { [] }

%inline any_word:
  | w = WORD { w }
  | w = UWORD { w }

%inline any:
  | w = any_word { w }
  | EQ { "=" }
