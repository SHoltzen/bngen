%{
(* Ocamlyacc header *)
open Printf
open CnParseTypes
open Mn.Factor.MP

let pline = ref 2 

let parse_error s =
  print_string "Parse error: ";
  print_endline s;
  printf "Line %d\n" !pline;
  flush stdout;;
%}

/* token declarations */
%token Tmn
%token Tbn
%token Tdn
%token Tfeatures
%token Ttable
%token Ttree
%token <int> Tint
%token <float> Tfloat
%token <bool * int * int> Tcond
%token <int * int> Tvar
%token <int> Tvarindex
%token Tlbrace
%token Trbrace
%token Tlparen
%token Trparen
%token TEOL
%token EOF

%type <CnParseTypes.pcn> cn

/* start symbols */
%start cn

%%

/* Ocamlyacc grammar and action rules */

mn:
| Tmn Tlbrace mncontents { $3 }
| Tmn TEOL Tlbrace mncontents { pline := !pline + 1; $4 }
;

cn: 
| Tbn Tlbrace cncontents { (true, $3) }
| Tdn Tlbrace cncontents { (false, $3) }
| Tbn TEOL Tlbrace cncontents { pline := !pline + 1; (true, $4) }
| Tdn TEOL Tlbrace cncontents { pline := !pline + 1; (false, $4) }
;

cncontents:
| Tvarindex Tlbrace mncontents cncontents { ($1, $3) :: $4 }
| Trbrace { [] }
| TEOL cncontents { pline := !pline + 1; $2 }
;

mncontents:
| Trbrace { [] }
| factor mncontents { $1 :: $2 }
| TEOL mncontents { pline := !pline + 1; $2 }
;

factor:
| Tfeatures Tlbrace featurelist { PFeatureSet $3 }
| Ttree     Tlbrace tree Trbrace     { PFeatureTree $3 }
| Ttree     Tlbrace tree TEOL Trbrace     { PFeatureTree $3 }
| Ttable    Tlbrace featurelist { PFeatureTable $3 }
| feature                       { PFeature $1 }
;

featurelist:
| feature featurelist { pline := !pline + 1; $1 :: $2}
| TEOL featurelist    { pline := !pline + 1; $2}
| Trbrace {[]}
| EOF {[]}
| error TEOL { pline := !pline + 1; []}
;

feature:
| Tint cond {((-1), float_of_int $1, $2)}
| Tfloat cond {((-1), $1, $2)}
| Tint Tfloat cond {($1, $2, $3)}
| Tint Tint cond {($1, float_of_int $2, $3)}
;

tree:
| Tfloat { PLeaf $1 }
| Tint   { PLeaf (float_of_int $1) }
| TEOL tree { pline := !pline + 1; $2 }
| Tlparen Tvar tree tree Trparen 
    { PVertex (fst $2, snd $2, $3, $4) }
;

cond:
| Tcond cond {$1 :: $2} 
| TEOL { pline := !pline + 1; []}
;

%%
