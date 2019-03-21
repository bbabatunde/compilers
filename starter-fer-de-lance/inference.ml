open Exprs
open Errors
open Printf
open Pretty
open Phases

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

let show_debug_print = ref false
let debug_printf fmt =
  if !show_debug_print
  then printf fmt
  else ifprintf stdout fmt
;;

type 'a envt = 'a StringMap.t;;
type 'a subst = (string * 'a) list;;


let print_schemeenv schemeenv =
  StringMap.iter (fun name scheme -> debug_printf "\t%s => %s\n" name (string_of_scheme scheme)) schemeenv;;
let print_env env =
  StringMap.iter (fun name typ -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) env;;

let print_subst subst =
  List.iter (fun (name, typ) -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) subst;;


let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)
             
let tInt = TyCon("Int", dummy_span)
let tBool = TyCon("Bool", dummy_span)
let intint2int = SForall([], TyArr([tInt; tInt], tInt, dummy_span), dummy_span)
let int2bool = SForall([], TyArr([tInt], tBool, dummy_span), dummy_span)
let tyVarX = TyVar("X", dummy_span)
let any2bool = SForall(["X"], TyArr([tyVarX], tBool, dummy_span), dummy_span)
let any2any = SForall(["X"], TyArr([tyVarX], tyVarX, dummy_span), dummy_span)
(* create more type synonyms here, if you need to *)
let initial_env : sourcespan scheme envt =
  List.fold_left (fun env (name, typ) -> StringMap.add name typ env) StringMap.empty [
      failwith "Create an initial function environment here"
  ]

let rec find_pos (ls : 'a envt) x pos : 'a =
  try
    StringMap.find x ls
  with
  | Not_found -> failwith (sprintf "Name %s not found at %s" x (string_of_sourcespan pos))
;;
let rec subst_var_typ ((tyvar, to_typ) as sub) in_typ =
  failwith "Implement substituting a type for a type variable, within a type, here"
;;
let subst_var_scheme ((tyvar, to_typ) as sub) scheme =
  failwith "Implement substituting a type for a type variable, within a scheme, here"
;;
let apply_subst_typ (subst : 'a typ subst) (t : 'a typ) : 'a typ =
  List.fold_left (fun t sub -> subst_var_typ sub t) t subst
;;
let apply_subst_scheme (subst : 'a typ subst) (scheme : 'a scheme) : 'a scheme =
  failwith "Implement applying a substitution to a scheme here"
;;
let apply_subst_env (subst : 'a typ subst) (env : 'a typ envt) : 'a typ envt =
  failwith "Implement applying a substitution to a type environment here"
;;
let apply_subst_schemeenv (subst : 'a typ subst) (env : 'a scheme envt) : 'a scheme envt =
  failwith "Implement applying a substitution to a scheme environment here"
;;
let apply_subst_subst (subst : 'a typ subst) (dest : 'a typ subst) : 'a typ subst =
  failwith "Implement applying a substitution to another substitution here"
;;
let compose_subst (sub1 : 'a typ subst) (sub2 : 'a typ subst) : 'a typ subst =
  sub1 @ (apply_subst_subst sub1 sub2)
;;
let rec ftv_type (t : 'a typ) : StringSet.t =
  match t with
  | TyBlank _ -> StringSet.empty
  | TyCon _ -> StringSet.empty
  | TyVar(name, _) -> StringSet.singleton name
  | TyArr(args, ret, _) ->
    List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
                    args
                    (ftv_type ret)
  | TyApp(typ, args, _) ->
    List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
                    args
                    (ftv_type typ)
  | TyTup _ -> raise (NotYetImplemented "Finish this!")
;;
let ftv_scheme (s : 'a scheme) : StringSet.t =
  match s with
  | SForall(args, typ, _) ->
     StringSet.diff (ftv_type typ) (StringSet.of_list args)
let ftv_env (e : 'a typ envt) : StringSet.t = 
  failwith "Compute the free type variables of an environment here"
;;
let occurs (name : string) (t : 'a typ) =
  StringSet.mem name (ftv_type t)
;;
exception OccursCheck of string
let bind (tyvarname : string) (t : 'a typ) : 'a typ subst =
  match t with
  | TyVar(name, _) when tyvarname = name -> [] (* nothing to be done *)
  | _ ->
     if StringSet.mem tyvarname (ftv_type t)
     then raise (OccursCheck (sprintf "Infinite types: %s occurs in %s" tyvarname (string_of_typ t)))
     else [(tyvarname, t)]
;;
let ty_err t1 t2 loc reasons = TypeMismatch(loc, t2, t1, reasons)


(* Is the given name n an abbreviation for some other type definition? *)
let is_abbrev (n : string) (tyenv : sourcespan typ envt) (loc : sourcespan) =
  StringMap.mem n tyenv &&
    match find_pos tyenv n loc with
    | TyCon(n', _) -> n <> n'
    | _ -> true;;

(* The tyenv here stores tydecls from the Program: it maps *type names* to type definitions.
   Use it in the TyCon case to expand any type synonyms you find.
*)
let rec unify tyenv (t1 : 'a typ) (t2 : 'a typ) (loc : sourcespan) (reasons : reason list) : 'a typ subst =
  failwith "Implement type unification"
;;     
     
let gensym =
  let count = ref 0 in
  let next () =
    count := !count + 1;
    !count
  in fun str -> sprintf "%s_%d" str (next ());;

(* Eliminates all `TyBlank`s in a type, and replaces them with fresh type variables *)
let rec unblank (t : 'a typ) : 'a typ =
  match t with
  | TyBlank tag -> TyVar(gensym "blank", tag)
  | TyCon _ -> t
  | TyVar _ -> t
  | TyArr(args, ret, tag) ->
     let args = List.map unblank args in
     let ret = unblank ret in TyArr(args, ret, tag)
  | TyApp(t, args, tag) ->
     let t = unblank t in
     let args = List.map unblank args in TyApp(t, args, tag)
  | TyTup(tys, tag) -> TyTup(List.map unblank tys, tag)
;;
let instantiate (s : 'a scheme) : 'a typ =
  failwith "Implement instantiating a type scheme here"
;;
let generalize (e : 'a typ envt) (t : 'a typ) : 'a scheme =
  failwith "Implement generalizing a type here"
;;

(* Ex 14 *)
(* NOTE: infer_exp no longer takes a funenv, because functions and variables now must share one namespace.
   But that means that our env must now contain schemes instead of mere typs -- which makes things awkward.
   You can work around this by: whenever you used to have a raw typ, now create SForall([], that-typ),
   and whenever you need to convert back from a scheme to a typ, just call instantiate.  
   Note that these intermediate schemes might be somewhat malformed, with free type variables in them.
   That's temporarily ok, since they were already checked for well-formedness, so those "free" type
   variables are bound within the overall context of inference.

   The tyenv argument is used to store the tydecls of the program: it maps *type names* to type
   definitions, rather than *variable names* to types.
*)
let rec infer_exp (tyenv : sourcespan typ StringMap.t) (env : sourcespan scheme envt) (e : sourcespan expr) reasons
        : (sourcespan typ subst * sourcespan typ * sourcespan expr) (* unification, result typ, rebuilt expr *)=
  match e with
  | EIf(c, t, f, loc) ->
     let (c_subst, c_typ, c) = infer_exp tyenv env c reasons in
     let env = apply_subst_schemeenv c_subst env in
     let (t_subst, t_typ, t) = infer_exp tyenv env t reasons in
     let env = apply_subst_schemeenv t_subst env in
     let (f_subst, f_typ, f) = infer_exp tyenv env f reasons in
     let env = apply_subst_schemeenv f_subst env in
     (* Compose the substitutions together *)
     let subst_so_far = compose_subst (compose_subst c_subst t_subst) f_subst in
     (* rewrite the types *)
     let c_typ = apply_subst_typ subst_so_far c_typ in
     let t_typ = apply_subst_typ subst_so_far t_typ in
     let f_typ = apply_subst_typ subst_so_far f_typ in
     (* unify condition with Bool *)
     let unif_subst1 = unify tyenv c_typ tBool loc reasons in
     (* unify two branches *)
     let unif_subst2 = unify tyenv t_typ f_typ loc reasons in
     (* compose all substitutions *)
     let final_subst = compose_subst (compose_subst subst_so_far unif_subst1) unif_subst2 in
     let final_typ = apply_subst_typ final_subst t_typ in
     (final_subst, final_typ, e)
  | _ -> failwith "Finish implementing inferring types for expressions"
;;

let infer_decl tyenv env (decl : sourcespan decl) reasons : sourcespan scheme envt * sourcespan typ * sourcespan decl =
  match decl with
  | DFun(name, args, scheme, body, loc) ->
     failwith "Implement inferring type schemes for declarations"
;;
let infer_group tyenv env (g : sourcespan decl list) : (sourcespan scheme envt * sourcespan decl list) =
  failwith "Implement inferring type schemes for declaration groups"
;;
let infer_prog tyenv env (p : sourcespan program) : sourcespan program =
  match p with
  | Program(declgroups, body, typ, tag) ->
     failwith "Implement inferrence for entire programs"
;;
let type_synth (p : sourcespan program) : sourcespan program fallible =
  try
    Ok(infer_prog initial_env StringMap.empty p)
  with e -> Error([e])
;;
