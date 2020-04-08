{
  open Parser
}

let spc = [' ']+
let str = ['a'-'z']+

rule read = parse
  | spc       { read lexbuf }
  | "->"      { ARROW }
  | ":"       { COLON }
  | "Bool"    { BOOL }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "\\"      { SLASH }
  | "."       { DOT }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "if"      { IF }
  | str as x  { SYM x }
  | eof       { EOF }
