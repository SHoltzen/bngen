{
open CnParseTypes;;
open CnParser;;
(* Raised when parsing ends *)
exception Eof;;

module L = Lexing
let linenum lexbuf = lexbuf.L.lex_curr_p.L.pos_lnum

let line = ref 1;;

let keywords = Hashtbl.create 10
let _ = 
  List.iter2 (Hashtbl.add keywords)
    ["mn"; "bn"; "dn"; "features"; "tree"; "table"; "eof"]
    [Tmn; Tbn; Tdn; Tfeatures; Ttree; Ttable; EOF];;
}

let digits = ['0'-'9']+
let identifier = ['a'-'z' 'A'-'Z']+

rule lexer = parse
(* eat blank characters *)
    [' ' '\t'] {lexer lexbuf}
(* | "Feature list:" {lexer lexbuf} *)
  | '{' {Tlbrace}
  | '}' {Trbrace}
  | '(' {Tlparen}
  | ')' {Trparen}
  | ('-')? "inf" {Tfloat( float_of_string(L.lexeme lexbuf))}
  | identifier {
      let x = String.lowercase (Lexing.lexeme lexbuf) in
      try Hashtbl.find keywords x
      with Not_found -> 
        failwith((Lexing.lexeme lexbuf) 
          ^ ": unknown identifier on line " ^ string_of_int (linenum lexbuf))}
  | digits {Tint (int_of_string (L.lexeme lexbuf))}
  | ('-')? digits ('.' digits)? (['e' 'E'] ['+' '-']? digits)? 
      {Tfloat( float_of_string(L.lexeme lexbuf))}
  | '+' 'v' (digits as var) '_' (digits as value) 
      {Tcond(true, int_of_string var, int_of_string value)}
  | '-' 'v' (digits as var) '_' (digits as value) 
      {Tcond(false, int_of_string var, int_of_string value)}
  | 'v' (digits as var) '_' (digits as value) 
      {Tvar( int_of_string var, int_of_string value)}
  | 'v' (digits as var)
      {Tvarindex(int_of_string var)}
  | ['\n' '\r']+ {L.new_line lexbuf; TEOL}
  | eof {EOF}
  | _ {failwith((L.lexeme lexbuf) ^ 
       ": mistake on line " ^ string_of_int lexbuf.L.lex_curr_p.L.pos_lnum)}
  
