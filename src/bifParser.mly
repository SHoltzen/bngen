%{
(* Ocamlyacc header *)
open BifParseType
open Printf
open Lexing

let parse_error s =
  print_string "Parse error: ";
  print_endline s;
  let pos = Parsing.symbol_end_pos () in
  printf "Line %d, offset %d\n" pos.pos_lnum 
    (pos.pos_cnum - pos.pos_bol);
  flush stdout
;;
%}

/* token declarations */
%token <float> Tnum
%token <BifParseType.bifNumStr> Tident
%token <int> Tint
%token Tnetwork
%token Tvariable
%token Ttype
%token Tproperty
%token Tprobability
%token Ttable
%token Tcomma
%token Tsemicolon
%token Tbar
%token Tlparen
%token Trparen
%token Tlbrace
%token Trbrace
%token Tlbracket
%token Trbracket
%token Tequals
%token TEOL
%token TEOF

%type <BifParseType.bif> bn
/* start symbol */
%start bn

%%

/* Ocamlyacc grammar and action rules */
bn:
  network bn     {match $2 with (n,v,p) -> ($1,v,p)}
| variable bn    {match $2 with (n,v,p) -> (n,$1::v,p)}
| probability bn {match $2 with (n,v,p) -> (n,v,$1::p)}
| TEOF {("",[],[])}
;

network: Tnetwork Tident Tlbrace netbody {$2.s};

netbody:
  property netbody {()}
| Trbrace          {()}
;

property:
  Tproperty Tident Tequals Tident Tsemicolon {()}
| Tproperty Tident Tequals Tlparen identlist Trparen Tsemicolon {()}
| Tproperty Tident Tequals Tnum Tsemicolon {()}
;

variable: Tvariable Tident Tlbrace varbody Trbrace {($2.s,$4)};

varbody:
  property varbody {$2}
| atype varbody {$1}
| atype {$1}
;

atype:
  Ttype Tident Tlbracket Tident Trbracket valnames {
    if $2.s <> "discrete" then 
      (parse_error "Only discrete variable types are supported" ; [])
    else begin
      if not $4.is_int then
        (parse_error "Number of variable states must be an integer" ; [])
      else if $4.i <> List.length $6 then
        (parse_error "Number of variable states does not match" ; [])
      else $6
    end
  }
;

valnames:
  Tlbrace identlist Trbrace Tsemicolon {$2}
| Tequals Tlbrace identlist Trbrace Tsemicolon {$3}
;

probability:
  Tprobability probheader Tlbrace probbody Trbrace {($2,$4)}
| Tprobability probheader Tlbrace probbody Trbrace Tsemicolon {($2,$4)}
;

probheader: 
  Tlparen Tident Tbar identlist Trparen {($2.s,$4)}
| Tlparen Tident Trparen {($2.s,[])}
;

probbody:
  property probbody {$2}
| probline probbody {$1::$2}
| probline          {[$1]}
| property          {[]}
;

probline: 
  Tlparen identlist Trparen floatlist Tsemicolon {($2,$4)}
| Ttable floatlist Tsemicolon {([],$2)}
;

identlist:
  Tident Tcomma identlist {$1.s::$3}
| Tident {[$1.s]}
;

floatlist:
  Tident Tcomma floatlist {
    if $1.is_float then 
      $1.f :: $3
    else if $1.is_int then 
      (float_of_int $1.i) :: $3
    else
      (parse_error "List element is not numeric" ; 0.0 :: $3)
  }
| Tident {
    if $1.is_float then 
      [$1.f]
    else if $1.is_int then 
      [(float_of_int $1.i)]
    else
      (parse_error "List element is not numeric" ; [0.0])
  }
;

%%
