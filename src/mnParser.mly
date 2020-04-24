%{
(* Ocamlyacc header *)
open Printf
open MnParseTypes

let pline = ref 2 

let parse_error s =
  print_string "Parse error: ";
  print_endline s;
  printf "Line %d\n" !pline;
  flush stdout;;
%}

/* token declarations */
%token Tmn
%token Tfeatures
%token Ttable
%token Ttree
%token <int> Tint
%token <float> Tfloat
%token <bool * int * int> Tcond
%token <int * int> Tvar
%token Tlbrace
%token Trbrace
%token Tlparen
%token Trparen
%token Tweight
%token TEOL
%token EOF

%type <MnParseTypes.pfeature> feature
%type <MnParseTypes.pfeaturelist> featurelist
%type <MnParseTypes.pmn> mn

/* start symbols */
%start feature
%start featurelist
%start mn

%%

/* Ocamlyacc grammar and action rules */

mn:
| Tmn Tlbrace mncontents { $3 }
| Tmn TEOL Tlbrace mncontents { pline := !pline + 1; $4 }
;

mncontents:
| Trbrace { {factors=[]; weights=[];} }
| weight mncontents { 
    {$2 with weights=$1 :: $2.weights} }
| factor mncontents { 
    {$2 with factors=$1 :: $2.factors} }
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
| Tfloat { MnParseTypes.PLeaf $1 }
| Tint   { MnParseTypes.PLeaf (float_of_int $1) }
| TEOL tree { pline := !pline + 1; $2 }
| Tlparen Tvar tree tree Trparen 
    { MnParseTypes.PVertex (fst $2, snd $2, $3, $4) }
;

cond:
| Tcond cond {$1 :: $2} 
| TEOL { pline := !pline + 1; []}
;

weight:
| Tweight Tint Tfloat {($2, $3)}
;

%%
