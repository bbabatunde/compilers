open Exprs
open Errors
open Printf
open Pretty
open Phases

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

type 'a envt = 'a StringMap.t;;
type 'a subst = (string * 'a) list;;


let print_funenv funenv =
  StringMap.iter (fun name scheme -> debug_printf "\t%s => %s\n" name (string_of_scheme scheme)) funenv;;
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
let rec subst_var_typ (((tyvar : string ), (to_typ: 'a typ)) as sub) (in_typ : 'a typ) : 'a typ =
  match in_typ with
  | TyBlank(pos) -> TyBlank(pos)
  | TyCon(str,pos)-> if str = tyvar then in_typ else TyBlank(pos)
  | TyVar(str,pos)-> if str = tyvar then in_typ else TyBlank(pos)
  | TyArr(typlist,typ,pos)-> TyArr((List.map (fun e -> (subst_var_typ sub e)) typlist),(subst_var_typ sub typ),pos)
  | TyApp(typ,typlist,pos)-> TyApp((subst_var_typ sub typ) , (List.map (fun e -> subst_var_typ sub e) typlist),pos)
 
;;
let  subst_var_scheme (((tyvar : string ), (to_typ: 'a typ)) as sub) (in_scheme : 'a scheme) : 'a scheme =
  match in_scheme with 
  |SForall(strlst, typ,pos)-> if List.mem tyvar strlst then in_scheme else SForall(strlst, (subst_var_typ sub typ), pos)
;;
let apply_subst_typ (subst : 'a typ subst) (t : 'a typ) : 'a typ =
  List.fold_left (fun t sub -> subst_var_typ sub t) t subst
;;
 let apply_subst_scheme (subst : 'a typ subst) (scheme : 'a scheme) : 'a scheme =
    List.fold_left (fun sch sub -> subst_var_scheme sub sch) scheme subst
;;
let apply_subst_env (subst : 'a typ subst) (env : 'a typ envt) : 'a typ envt =
  StringMap.map (fun typ -> apply_subst_typ subst typ) env 
;;
let apply_subst_funenv (subst : 'a typ subst) (env : 'a scheme envt) : 'a scheme envt =
  StringMap.map (fun sch -> apply_subst_scheme subst sch) env 
;;
let apply_subst_subst (subst : 'a typ subst) (dest : 'a typ subst) : 'a typ subst =
   List.map (fun (name,typ) ->  (name, (apply_subst_typ subst typ))) dest
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
;;
let ftv_scheme (s : 'a scheme) : StringSet.t =
  match s with
  | SForall(args, typ, _) -> StringSet.diff (ftv_type typ) (StringSet.of_list args)
(*  redo *)
let ftv_env (e : 'a typ envt) : StringSet.t = 
  failwith "implement ftv_env"
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
let rec unify (t1 : 'a typ) (t2 : 'a typ) (loc : sourcespan) (reasons : reason list) : 'a typ subst = match (t1 , t2) with 
  | TyBlank(pos),TyBlank(pos2) -> []
  | TyCon(str,pos),TyCon(str2,pos2) ->   if str = str2 then [] else raise (ty_err t1 t2 loc reasons) 
  | TyVar(str,pos),TyVar(str2,pos2) ->   bind str t2 
  | TyArr(typlist, typ,pos),TyArr(typlist2,typ2, pos2) -> if List.length typlist = List.length typlist2
                                                          then (List.flatten (List.map2 (fun e1 e2  ->  unify e1 e2 loc reasons) typlist typlist2)) @ 
                                                          (unify typ typ2 loc reasons) 
                                                          else raise (ty_err  t1 t2 loc reasons) 
  | TyApp(typ,typlist,pos), TyApp(typ2, typlist2,pos2)->  failwith "implement unify for TyApp" 
  |_ -> raise (ty_err t1 t2 loc reasons) 

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
;;
let instantiate (s : 'a scheme) : 'a typ = match s with 
 |SForall(strlst, typ,pos) -> unblank typ
;;

let generalize (e : 'a typ envt) (t : 'a typ) : 'a scheme =
     SForall( StringSet.elements (StringSet.diff (ftv_env e) (ftv_type t)) ,TyVar(gensym "blank",dummy_span),dummy_span)
;;

(* Ex 14 *)
let rec infer_exp (funenv : sourcespan scheme envt) (env : sourcespan typ envt) (e : sourcespan expr) reasons
        : (sourcespan typ subst * sourcespan typ * sourcespan expr) (* unification, result typ, rebuilt expr *)=
  match e with
  | EIf(c, t, f, loc) ->
     let (c_subst, c_typ, c) = infer_exp funenv env c reasons in
     let (t_subst, t_typ, t) = infer_exp funenv env t reasons in
     let (f_subst, f_typ, f) = infer_exp funenv env f reasons in
     (* Compose the substitutions together *)
     let subst_so_far = compose_subst (compose_subst c_subst t_subst) f_subst in
     (* rewrite the types *)
     let c_typ = apply_subst_typ subst_so_far c_typ in
     let t_typ = apply_subst_typ subst_so_far t_typ in
     let f_typ = apply_subst_typ subst_so_far f_typ in
     (* unify condition with Bool *)
     let unif_subst1 = unify c_typ tBool loc reasons in
     (* unify two branches *)
     let unif_subst2 = unify t_typ f_typ loc reasons in
     (* compose all substitutions *)
     let final_subst = compose_subst (compose_subst subst_so_far unif_subst1) unif_subst2 in
     let final_typ = apply_subst_typ final_subst t_typ in
     (final_subst, final_typ, e)
  | _ -> failwith "Finish implementing inferring types for expressions"
;;

let infer_decl funenv env (decl : sourcespan decl) reasons : sourcespan scheme envt * sourcespan typ * sourcespan decl =
  match decl with
  | DFun(name, args, scheme, body, loc) ->
     failwith "Implement inferring type schemes for declarations"
;;
let infer_group funenv env (g : sourcespan decl list) : (sourcespan scheme envt * sourcespan decl list) =
  failwith "Implement inferring type schemes for declaration groups"
;;
let infer_prog funenv env (p : sourcespan program) : sourcespan program =
  match p with
  | Program(declgroups, body, typ, tag) ->
     failwith "Implement inferrence for entire programs"
;;
let type_synth (p : sourcespan program) : sourcespan program fallible =
  try
    Ok(infer_prog initial_env StringMap.empty p)
  with e -> Error([e])
;;