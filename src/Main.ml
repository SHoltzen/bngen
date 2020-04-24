open Core

type typ =
    TCategory of int
  | TBool

type expr =
    True
  | False
  | Ident of string
  | Flip of float
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | Int of int * int (* value, size *)
  | Ite of expr * expr * expr
  | Category of float List.t
  | Tuple of expr * expr

type letelem = string * typ * expr

let sum_arr (a : float list) : float =
  List.fold ~init:(0.0) a ~f:(fun cur acc -> acc +. cur)

let mk_name varname idx =
  Format.sprintf "%s%i" varname idx

let rec list_n n : int list =
  if n < 0 then [] else
  match n with
    0 -> [0]
  | n -> n :: list_n (n-1)

exception NoMoreStates

(** Generates an assignment for the variable `var` *)
let var_assgn (network : Bn.network) (var : Bn.variable) : letelem =
  let idx = var.idx in
  let parents = Array.get network.parents idx in
  let cur_schema = Bn.schema network in
  let rec helper (parents: int list) (parent_values: int array) : expr =
    match parents with
      [] ->
      let catvalues = Bn.cond_prob network parent_values idx in
      (* add special case for flip *)
      if Array.length catvalues = 2 then
        Flip(Array.get catvalues 0)
      else
        Category(Array.to_list catvalues)
    | curparent::xs ->
      (* for each possible assignment to the parent, get a sub-expression *)
      let assignments = list_n ((Array.get cur_schema curparent)-1) in
      let num_assgn = List.length assignments in
      (* add special case for Boolean parent *)
      if (Array.get cur_schema curparent) = 2 then
        (Array.set parent_values curparent 0;
         let p1 = helper xs parent_values in
         Array.set parent_values curparent 1;
         let p2 = helper xs parent_values in
         Ite(Ident(Bn.varname network curparent), p1, p2))
      else
        (Array.set parent_values curparent (List.hd_exn assignments);
        List.fold (List.tl_exn assignments) ~init:(helper xs parent_values)
          ~f:(fun acc assgn ->
              Array.set parent_values curparent assgn;
              let subexpr = helper xs parent_values in
              Ite(Eq(Ident(Bn.varname network curparent), Int(assgn, num_assgn)), subexpr, acc)
            ))
  in let arr = Array.create (Array.length cur_schema) 0 in
  (* bool special case *)
  let curlen = Array.get cur_schema idx in
  if curlen = 2 then
    (var.vname, TBool, (helper parents arr))
  else
    (var.vname, TCategory(curlen), helper parents arr)




(**********************************************************************************)
(* pretty printing *)

let rec sym_string_of_expr (e:expr) : string =
  match e with
  | True -> "true"
  | False -> "false"
  | Ident(s) -> s
  | Flip(f) ->
     Format.sprintf "flip %f" f
  | Not(Ident(s)) -> Format.sprintf "! (%s)" s
  | And(e1, e2) -> Format.sprintf "(%s && %s)" (sym_string_of_expr e1) (sym_string_of_expr e2)
  | Or(e1, e2) -> Format.sprintf "(%s || %s)" (sym_string_of_expr e1) (sym_string_of_expr e2)
  | Ite(g, t, e) -> Format.sprintf "if (%s) then (%s) else (%s)" (sym_string_of_expr g)
                      (sym_string_of_expr t) (sym_string_of_expr e)
  | Category(l) ->
    let res = List.foldi l ~init:(Format.sprintf "discrete(") ~f:(fun idx acc i ->
        if idx = 0 then
          Format.sprintf "%s%f" acc i
        else
          Format.sprintf "%s,%f" acc i
      ) in
    Format.sprintf "%s)" res
  | Tuple(e1, e2) ->
    let s1 = sym_string_of_expr e1 in
    let s2 = sym_string_of_expr e2 in
    Format.sprintf "(%s,%s)" s1 s2
  | Eq(e1, e2) ->
    let s1 = sym_string_of_expr e1 in
    let s2 = sym_string_of_expr e2 in
    Format.sprintf "(%s == %s)" s1 s2
  | Int(v, sz) -> Format.sprintf "int(%d, %d)" sz v
  | _ -> failwith "unidentified"

let rec psi_string_of_expr (name: string) (e:expr) : string =
  match e with
  | True -> Format.sprintf "%s = true;" name
  | False -> Format.sprintf "%s = false;" name
  | Ident(s) -> s
  | Flip(f) ->
     Format.sprintf "%s = flip(%f);" name f
  | Ite(g, t, e) -> Format.sprintf "if (%s) { %s } else { %s }" (psi_string_of_expr name g)
                      (psi_string_of_expr name t) (psi_string_of_expr name e)
  | Category(l) ->
    let res = List.foldi l ~init:(Format.sprintf "%s = categorical([" name) ~f:(fun idx acc i ->
        if idx = 0 then
          Format.sprintf "%s%f" acc i
        else
          Format.sprintf "%s,%f" acc i
      ) in
    Format.sprintf "%s]);" res
  | Tuple(e1, e2) ->
    let s1 = psi_string_of_expr name e1 in
    let s2 = psi_string_of_expr name e2 in
    Format.sprintf "(%s,%s)" s1 s2
  | Int(v, _) -> string_of_int v
  | Eq(e1, e2) -> Format.sprintf "%s == %s" (psi_string_of_expr name e1) (psi_string_of_expr name e2)


let rec string_of_typ (t: typ) : string =
  match t with
    TCategory(n) -> Format.sprintf "Category(%d)" n
  | TBool -> Format.sprintf "Bool"


let ceil x top =
  if x >= top then top else x

let rec flatten_lst l =
  List.fold l ~init:[] ~f:(fun acc i ->
      List.append acc i
    )


let print_psi fname =
  let f = open_in fname in
  Format.printf "def main(){";
  let network = Bn.load_bif f in
  let p = List.map (Array.to_list network.topo_vars) ~f:(fun var ->
              var_assgn network var
    ) in
  List.iter p ~f:(fun (ident, typ, body) ->
      Format.printf "%s := 0; %s\n" ident (psi_string_of_expr ident body)
    );
  let (id, typ, body)::xs = p in
  let tuple : expr = List.fold xs ~init:(Ident(id)) ~f:(fun acc (id, _, _) ->
      Tuple(Ident(id), acc)
    ) in
  (* print tuple of results *)
  Format.printf "return %s;\n}\n" (sym_string_of_expr tuple);
  ()

let print_sym fname =
  let f = open_in fname in
  let network = Bn.load_bif f in
  let p = List.map (Array.to_list network.topo_vars) ~f:(fun var ->
              var_assgn network var
    ) in
  List.iter p ~f:(fun (ident, typ, body) ->
      Format.printf "let %s = %s in\n" ident (sym_string_of_expr body)
    );
  let (id, typ, body)::xs = p in
  let tuple : expr = List.fold xs ~init:(Ident(id)) ~f:(fun acc (id, _, _) ->
      Tuple(Ident(id), acc)
    ) in
  (* print tuple of results *)
  Format.printf "%s" (sym_string_of_expr tuple);
  ()

(* parse bif *)
let () =
  let ftype = Array.get Sys.argv 1 in
  let fname = Array.get Sys.argv 2 in
  match ftype with
    "psi" -> print_psi fname
  | "sym" -> print_sym fname
  | _ -> failwith "uncrecognized arg type"
