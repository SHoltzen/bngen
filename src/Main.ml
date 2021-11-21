open Core

type typ =
    TCategory of int
  | TBool

type expr =
    True
  | False
  | Ident of string (* hold the size *)
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

let log2 a = log a /. (log 2.0)
let num_binary_digits d = int_of_float (Float.round_down (log2 (float_of_int (d)))) + 1

(** Generates an assignment for the variable `var` *)
let var_assgn (network : Bn.network) (var : Bn.variable) : letelem =
  let idx = var.idx in
  let parents = Array.get network.parents idx in
  let cur_schema = Bn.schema network in
  let rec helper (parents: int list) (parent_values: int array) : expr =
    match parents with
      [] ->
      let catvalues = Bn.cond_prob network parent_values idx in
      Category(Array.to_list catvalues)
    | curparent::xs ->
      (* for each possible assignment to the parent, get a sub-expression *)
      let assignments = list_n ((Array.get cur_schema curparent)-1) in
      let num_assgn = List.length assignments in
      (* add special case for Boolean parent *)
      (Array.set parent_values curparent (List.hd_exn assignments);
       List.fold (List.tl_exn assignments) ~init:(helper xs parent_values)
         ~f:(fun acc assgn ->
             Array.set parent_values curparent assgn;
             let subexpr = helper xs parent_values in
             Ite(Eq(Ident(Bn.varname network curparent), Int(assgn, num_binary_digits (num_assgn - 1))), subexpr, acc)
           ))
  in let arr = Array.create (Array.length cur_schema) 0 in
  (* bool special case *)
  let curlen = Array.get cur_schema idx in
  (var.vname, TCategory(curlen), helper parents arr)



(** simppl stuff *)
type stmt =
    Ite of expr * stmt * stmt
  | Assgn of string * expr
  | SFlip of string * float
  | Seq of stmt * stmt
  | Skip

(** convert a variable and its name into the identifier *)
let var_ident name value =
  Format.sprintf "%s%d" name value

let var_stmt (network: Bn.network) (var: Bn.variable) : stmt =
  let idx = var.idx in
  let parents = Array.get network.parents idx in
  let cur_schema = Bn.schema network in
  let rec helper (parents: int list) (parent_values: int array) : stmt =
    match parents with
      [] ->
      let catvalues = Array.to_list (Bn.cond_prob network parent_values idx) in
      (* fold over the categoricals, generating a list of assignments for each
         possible value *)
      let rec build_assgn values idx cur_z =
        (match values with
         | [] -> Skip
         | prob::vs ->
           let new_z = cur_z -. prob in
           let cur_name = var_ident var.vname idx in
           if (Float.(<=.) cur_z 0.0) then
             Seq(Assgn(cur_name, False), build_assgn vs (idx+1) cur_z)
           else Seq(SFlip(cur_name, prob /. cur_z), Ite(Ident(cur_name),
                                                        build_assgn vs (idx+1) (-1.0), (* z <= 0 means all false*)
                                                        build_assgn vs (idx+1) new_z))
        ) in
      build_assgn catvalues 0 1.0
    | curparent::xs ->
      (* for each possible assignment to the parent, get a sub-expression *)
      let assignments = list_n ((Array.get cur_schema curparent)-1) in
      (* add special case for Boolean parent *)
      (Array.set parent_values curparent (List.hd_exn assignments);
       List.fold (List.tl_exn assignments) ~init:(helper xs parent_values)
         ~f:(fun acc assgn ->
             Array.set parent_values curparent assgn;
             let subexpr = helper xs parent_values in
             Ite(Ident(var_ident (Bn.varname network curparent) assgn), subexpr, acc)
           ))
  in let arr = Array.create (Array.length cur_schema) 0 in
  helper parents arr




(**********************************************************************************)
(* pretty printing *)

let rec sym_string_of_expr (e:expr) : string =
  match e with
  | True -> "true"
  | False -> "false"
  | Ident(s) -> s
  | Flip(f) ->
    failwith " no flips "
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

let rec simppl_string_of_stmt (s:stmt) : string = match s with
  | Skip -> "observe true"
  | SFlip(s, p) -> Format.sprintf "%s ~ flip %f" s p
  | Seq(s1, s2) -> Format.sprintf "%s;\n%s" (simppl_string_of_stmt s1) (simppl_string_of_stmt s2)
  | Assgn(s, e) -> Format.sprintf "%s = %s" s (sym_string_of_expr e)
  | Ite(e, thn, els) -> Format.sprintf "if %s {\n %s }\nelse {\n%s}"
      (sym_string_of_expr e)
      (simppl_string_of_stmt thn)
      (simppl_string_of_stmt els)



let rec psi_string_of_expr (name: string) (e:expr) : string =
  match e with
  | True -> Format.sprintf "%s = true;" name
  | False -> Format.sprintf "%s = false;" name
  | Ident(s) -> s
  | And(e1, e2) -> Format.sprintf "(%s && %s)"
                     (psi_string_of_expr name e1) (psi_string_of_expr name e2)
  | Or(e1, e2) -> Format.sprintf "(%s || %s)"
                     (psi_string_of_expr name e1) (psi_string_of_expr name e2)
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


let print_simppl fname =
  let f = open_in fname in
  Format.printf "def main(){";
  let network = Bn.load_bif f in
  let p = List.map (Array.to_list network.topo_vars) ~f:(fun var ->
      var_stmt network var
    ) in
  List.iter p ~f:(fun s ->
      Format.printf "%s\n" (simppl_string_of_stmt s)
    );
  (* let (id, typ, body)::xs = p in
   * let tuple : expr = List.fold xs ~init:(Ident(id)) ~f:(fun acc (id, _, _) ->
   *     Tuple(Ident(id), acc)
   *   ) in *)
  (* print tuple of results *)
  (* let (last_id, _, _) = List.last_exn p in *)
  (* Format.printf "%s\n}\n" (psi_string_of_expr "" tuple); *)
  ()


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
  (* let (last_id, _, _) = List.last_exn p in *)
  Format.printf "%s\n}\n" (psi_string_of_expr "" tuple);
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
  let tuple : expr = List.fold xs ~init:(Ident(id)) ~f:(fun acc (id, typ, _) ->
      match typ with
      | TCategory(sz) -> Tuple(Ident(id), acc)
      | TBool -> Or(Ident(id), acc)
    ) in
  (* print tuple of results *)
  (* let (last_id, _, _) = List.last_exn p in *)
  Format.printf "%s"  (sym_string_of_expr tuple);
  ()

(* parse bif *)
let () =
  let ftype = Array.get Sys.argv 1 in
  let fname = Array.get Sys.argv 2 in
  match ftype with
    "psi" -> print_psi fname
  | "sym" -> print_sym fname
  | "simppl" -> print_simppl fname
  | _ -> failwith "uncrecognized arg type"
