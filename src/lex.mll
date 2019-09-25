{
open Lets
}

let white = [' ' '\n' '\t' '\r']
let not_white = [^ ' ' '\n' '\t' '\r']

rule word = parse
  | white+ { word lexbuf }
  | not_white+ as s { match s with "let" -> LET | "=" -> EQ | s ->
                      if s.[0]='_' then UWORD s else WORD s }
  | eof { EOF }
