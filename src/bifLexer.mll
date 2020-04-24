{
open Lexing;;
open BifParseType;;
open BifParser;;
(* Raised when parsing ends *)
exception Eof;;

(* let start_next_line lexbuf = () *)

let start_next_line lexbuf =
   let lcp = lexbuf.lex_curr_p in
   lexbuf.lex_curr_p <- {
      lcp with
      pos_lnum = lcp.pos_lnum + 1;
      pos_bol = lcp.pos_cnum;}
}

let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '-' '%']
let nbegin = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '-' '%']
let digits = ['0'-'9']+
let ident = alnum+
let nl = "\n\r" | "\n" | "\r"

rule lexer = parse
(* eat blank characters *)
    [' ' '\t'] {lexer lexbuf}
  | ("//" [^ '\n''\r']*)? nl {start_next_line lexbuf; lexer lexbuf(*; TEOL*)}
  | digits {Tident(i_ident(Lexing.lexeme lexbuf))}
  | digits '.'? digits? (['e' 'E'] '-'? digits)? 
      {Tident(f_ident(Lexing.lexeme lexbuf))}
  | "network" {Tnetwork}
  | "variable" {Tvariable}
  | "probability" {Tprobability}
  | "table" {Ttable}
  | "property" {Tproperty}
  | "type" {Ttype}
  | '"' ([^'"']*) as s '"' {Tident(s_ident s)}
  | ident {Tident(s_ident(Lexing.lexeme lexbuf))}
  | '=' {Tequals}
  | ',' {Tcomma}
  | ';' {Tsemicolon}
  | '|' {Tbar}
  | '(' {Tlparen}
  | ')' {Trparen}
  | '{' {Tlbrace}
  | '}' {Trbrace}
  | '[' {Tlbracket}
  | ']' {Trbracket}
  | _ {let lcp = lexbuf.lex_curr_p in
       failwith((Lexing.lexeme lexbuf) ^ 
       ": line " ^ string_of_int lcp.pos_lnum ^
       ", offset " ^ string_of_int (lcp.pos_cnum - lcp.pos_bol))}
  | eof {TEOF}
