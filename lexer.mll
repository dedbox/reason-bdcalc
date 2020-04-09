{
  open Parser

  exception Error of string

}

let spc = [' ' '\t']+
let sym = ['a'-'z']+
let endl = '\r' | '\n' | "\r\n"

rule read = parse
  | spc       { read lexbuf }
  | endl      { Lexing.new_line lexbuf; read lexbuf }
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
  | sym as x  { SYM x }
  | eof       { EOF }
  | _  { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
