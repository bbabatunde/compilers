open Exprs
open Errors
open Printf
open Pretty
open Phases

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

let show_debug_print = ref true
let debug_printf fmt =
  if !show_debug_print
  then printf fmt
  else ifprintf stdout fmt
;;

type 'a envt = 'a StringMap.t;;
type 'a subst = (string * 'a) list;;


let print_funenv funenv =
    debug_printf "START_PRINT_FUNENV\n";
    StringMap.iter (fun name scheme -> debug_printf "\t%s => %s\n" name (string_of_scheme scheme)) funenv;
    debug_printf "END_PRINT_FUNENV\n";;

let print_env env =
    debug_printf "START_PRINT_ENV\n";
    StringMap.iter (fun name typ -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) env;
    debug_printf "END_PRINT_ENV\n";;

let print_set s =
    debug_printf "START_PRINT_SET\n";
    StringSet.iter  (fun ele -> debug_printf "%s\n" ele) s;
    debug_printf "END_PRINT_SET\n";;

let print_subst subst =
    debug_printf "START_PRINT_SUBST";
  List.iter (fun (name, typ) -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) subst;
    debug_printf "END_PRINT_SUBST";;


let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)
             
let tInt = TyCon("Int", dummy_span)
let tBool = TyCon("Bool", dummy_span)
let intint2int = SForall([], TyArr([tInt; tInt], tInt, dummy_span), dummy_span)
let int2bool = SForall([], TyArr([tInt], tBool, dummy_span), dummy_span)
let boolbool2bool = SForall([], TyArr([tBool; tBool], tBool, dummy_span), dummy_span)

let bool2bool = SForall([], TyArr([tBool], tBool, dummy_span), dummy_span)

let int2int = SForall([], TyArr([tInt], tInt, dummy_span), dummy_span)
let tyVarX = TyVar("X", dummy_span)
let any2bool = SForall(["X"], TyArr([tyVarX], tBool, dummy_span), dummy_span)
let any2any = SForall(["X"], TyArr([tyVarX], tyVarX, dummy_span), dummy_span)
let anyany2bool = SForall(["X"], TyArr([tyVarX; tyVarX], tBool, dummy_span), dummy_span)
(* create more type synonyms here, if you need to *)
let initial_env : sourcespan scheme envt =
  List.fold_left (fun env (name, typ) -> StringMap.add name typ env) StringMap.empty [
      ("add1", int2int);
      ("sub1", int2int);
      ("print", any2any);
      ("isbool", any2bool);
      ("isnum", any2bool);
      ("not", bool2bool);
      ("plus", intint2int);
      ("minus", intint2int);
      ("and", boolbool2bool);
      ("or", boolbool2bool);
      ("greater", boolbool2bool);
      ("greaterq", boolbool2bool);
      ("less", boolbool2bool);
      ("lesseq", boolbool2bool);
      ("eq", anyany2bool);
      ("eqb", boolbool2bool);
      ("times", intint2int);
      ("printb", any2any);
      ("istuple", any2bool);
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
  | TyCon(str, pos)-> in_typ
  | TyVar(str, pos)-> if str = tyvar then to_typ else in_typ
  | TyArr(typlist, typ, pos)-> TyArr((List.map (fun e -> (subst_var_typ sub e)) typlist),(subst_var_typ sub typ), pos)
  | TyApp(typ, typlist, pos)-> TyApp((subst_var_typ sub typ) , (List.map (fun e -> subst_var_typ sub e) typlist), pos)
  | TyTup(lst, pos) -> TyTup((List.map (fun e -> (subst_var_typ sub e)) lst), pos)
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
  | TyTup(lst, _) ->
    List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
                    lst
                    StringSet.empty
;;
let ftv_scheme (s : 'a scheme) : StringSet.t =
  match s with
  | SForall(args, typ, _) -> StringSet.diff (ftv_type typ) (StringSet.of_list args)
;;

let ftv_env (e : 'a typ envt) : StringSet.t =
    List.fold_right (fun ele acc -> StringSet.union ele acc)
        (List.map (fun (k, t) -> (ftv_type t)) (StringMap.bindings e))
        StringSet.empty
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

let ty_err t1 t2 loc reasons = TypeMismatch(loc, t1, t2, reasons);;

let rec unify (t1 : 'a typ) (t2 : 'a typ) (loc : sourcespan) (reasons : reason list) : 'a typ subst =
    match t1 with
    | TyVar(varname, pos) -> [(varname, t2)]
    | TyCon(conname, pos) ->
        (match t2 with
        | TyCon(ty2, pos) -> if conname = ty2 then [] else (debug_printf "teehee"; raise (TypeMismatch(pos, t1, t2, reasons)))
        | TyVar(varname, pos) -> [(varname, t1)]
        | _ -> raise (TypeMismatch(pos, t1, t2, reasons)))
    | TyArr(l1, ty1, pos) ->
        (match t2 with
        | TyArr(l2, ty2, pos) -> if ((List.length l1) != (List.length l2)) then raise (TypeMismatch(pos, t1, t2, reasons)) else
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2)) @
            (unify ty1 ty2 pos reasons)
        | _ -> raise (TypeMismatch(pos, t1, t2, reasons)))
    | TyApp(appTyp, l1, pos) ->
        (match t2 with
        | TyApp(ty2, l2, pos) ->
            (unify appTyp ty2 pos reasons) @
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2))
        | _ -> raise (TypeMismatch(pos, t1, t2, reasons)))
    | TyTup(l1, pos) ->
        (match t2 with
        | TyTup(l2, pos) ->
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2))
        | _ -> raise (TypeMismatch(pos, t1, t2, reasons)))
    | TyBlank(pos) -> raise (TypeMismatch(pos, t1, t2, reasons))

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
  | TyTup(lst, tag) ->
     let lst = List.map unblank lst in TyTup(lst, tag)
;;

let rec replace_in_type rep_lst t =
    match t with
    | TyCon _ -> t
    | TyVar(name, pos) ->
        let replacement = rep_lookup name rep_lst in
        if (String.equal replacement "") then t
        else TyVar(replacement, pos)
    | TyArr(lst, typ, pos) ->
        TyArr((List.map (fun e -> replace_in_type rep_lst e) lst), (replace_in_type rep_lst typ), pos)
    | TyApp(typ, lst, pos) ->
        TyApp((replace_in_type rep_lst typ), (List.map (fun e -> replace_in_type rep_lst e) lst), pos)
    | TyTup(lst, pos) ->
        TyTup((List.map (fun e -> replace_in_type rep_lst e) lst), pos)
    | TyBlank(_) -> raise (InternalCompilerError "Unblank broken? o.O")
and rep_lookup name lst =
    match lst with
    | [] -> ""
    | (old_name, TyVar(new_name, _))::rest ->
        if old_name = name then new_name else rep_lookup name rest
    | _ -> raise (InternalCompilerError "Weird call to replace in type, should not have no tyvar pairs.")
;;

let instantiate (s: 'a scheme) : 'a typ =
    match s with
    | SForall(inp_lst, op_typ, pos) ->
            let inp_lst_typ = List.fold_left (fun acc ele -> (acc @ [(ele, TyVar(gensym "init", dummy_span))])) [] inp_lst in
            let op_typ = replace_in_type inp_lst_typ (unblank op_typ) in
            debug_printf "\ninit to: %s\n" (string_of_typ op_typ);
            op_typ
;;

let generalize (e : 'a typ envt) (t : 'a typ) : 'a scheme =
    (* collect all the free type variables in the type of the function body *)
    (* subtract away any type variables that appear free in the type environment *)
    let (arg_typ_lst, fun_bod) = 
        (match t with
        | TyArr(arg_typ_lst, bod_typ, loc) -> (arg_typ_lst, bod_typ)
        | _ -> raise (InternalCompilerError "Fn type not type Arr o.O")) in
    let _ = List.iter
        (fun t -> debug_printf "arg_typ: %s\n" (string_of_typ t)) arg_typ_lst in
    let arg_typ_lst = List.fold_left
        (fun acc ele -> StringSet.union acc (ftv_type ele)) StringSet.empty arg_typ_lst in
    print_set arg_typ_lst;
    let ftv_t = ftv_type fun_bod in
    let ftv_e = ftv_env e in
    print_set ftv_t;
    print_set ftv_e;
    let leftover_free_vars = StringSet.elements(StringSet.union arg_typ_lst (StringSet.diff (ftv_type t) (ftv_env e))) in
    SForall(leftover_free_vars, t, dummy_span)
;;

let opname op =
    match op with
    (* Prim1s *)
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | IsBool -> "isbool"
    | IsNum -> "isnum"
    | IsTuple -> "istuple"
    | Not -> "not"
    | _ -> raise (InternalCompilerError "Invalid op name :/")
;;
    (* Prim2s *)
let opname2 op =
    match op with
    | Plus -> "plus"
    | Minus -> "minus"
    | Times -> "times"
    | And -> "and"
    | Or -> "or"
    | Greater -> "greater"
    | GreaterEq -> "greatereq"
    | Less -> "less"
    | LessEq -> "lesseq"
    | Eq ->  "eq"
;;

(* Ex 14 *)
let rec infer_exp (funenv : sourcespan scheme envt) (env : sourcespan typ envt) (e : sourcespan expr) reasons
        : (sourcespan typ subst * sourcespan typ * sourcespan expr) (* unification, result typ, rebuilt expr *)=
  match e with
  | EIf(c, t, f, loc) ->
     let (c_subst, c_typ, c) = infer_exp funenv env c reasons in
     let env = apply_subst_env c_subst env in (****************************** NEW *)
     let (t_subst, t_typ, t) = infer_exp funenv env t reasons in
     let env = apply_subst_env t_subst env in (****************************** NEW *)
     let (f_subst, f_typ, f) = infer_exp funenv env f reasons in
     let _ = apply_subst_env f_subst env in (****************************** NEW *)
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
  | ELet(binds, exp,loc) -> 
          (* infer type for bindings while building up typ env *)
          let (new_env, new_subst) = 
              (* Typecheck the let binding itself in the original content.*)
              (* process each binding and add to env while tracking subs *)
              List.fold_left (fun acc ele ->
                  let (env, subs) = acc in
                  let (btype, e, l) = ele in
                  (match btype with
                    | BName(n, t, l) ->
                        (* Process expr and get subs *)
                        let (e_sub, e_type, _) = infer_exp funenv env e reasons in
                        (* Throw out the annotated type, should be blank. *)
                        (* Add new computed binding to env *)
                        let new_env = StringMap.add n e_type env in
                        (new_env, e_sub)
                    | BBlank _ -> (env, []) (*Ignoring type inference on blanked let, means nothing to bod type.*)
                    | _ -> raise (InternalCompilerError "Binding of non variable name found :/ Desugar much?"))) 
              (env, []) binds in
          (* infer type on body to get eesult type *)
          let (exp_subst, exp_type, _) = infer_exp funenv new_env exp reasons in
          (* resulting substitution is final substitution *)
          let final_subst = compose_subst new_subst exp_subst in
          (* see if constraints have a solution *)
          let exp_type = apply_subst_typ final_subst exp_type in
          (final_subst, exp_type, e)
  | EPrim1(op, exp, loc) ->  
          (* Lookup the relevant scheme *)
          let looked_up_scheme = find_pos funenv (opname op) loc in
          (* Instantiate it to a type *)
          let looked_up_tyarr = instantiate looked_up_scheme in
          debug_printf "\nlooked_up_tyarr: %s\n" (string_of_typ looked_up_tyarr);
          (* Infer a type for the argument(s) of the primitive. *)
          let (subs, infered_arg_typ, _) = infer_exp funenv env exp reasons in
          debug_printf "\ninfered_arg_typ: %s\n" (string_of_typ infered_arg_typ);
          (* Make up a brand-new type variable for the return type of the operation. *)
          let new_return_tyvar = TyVar(gensym "prim1res", loc) in
          (* Construct a new arrow type using the inferred types of the argument(s) and the made-up return type variable. *)
          let new_op_tyarr = TyArr([infered_arg_typ], new_return_tyvar, loc) in
          debug_printf "\nnew_op_tyarr: %s\n" (string_of_typ new_op_tyarr);
          (* Recursively unify the looked-up arrow type of the operator, with the constructed arrow type. *)
          let unify_subs = unify looked_up_tyarr new_op_tyarr loc reasons in
          debug_printf "\nUNIFIED SUBS\n";
          print_subst unify_subs;
          (* If all goes well, return the newly-constructed return type variable, and the substitution obtained from the recursive unification call. *)
          let all_subs = compose_subst unify_subs  subs in
          let new_return_tyvar = apply_subst_typ all_subs new_return_tyvar in
          (all_subs, new_return_tyvar, e)
  | EPrim2(op, exp1, exp2, loc) ->
          (* Lookup the relevant scheme *)
          let looked_up_scheme = find_pos funenv (opname2 op) loc in
          (* Instantiate it to a type *)
          let looked_up_tyarr = instantiate looked_up_scheme in
          debug_printf "\nlooked_up_tyarr: %s\n" (string_of_typ looked_up_tyarr);
          (* Infer a type for the argument(s) of the primitive. *)
          let (subs1, infered_arg_typ1, _) = infer_exp funenv env exp1 reasons in
          debug_printf "\ninfered_arg_typ1: %s\n" (string_of_typ infered_arg_typ1);
          let (subs2, infered_arg_typ2, _) = infer_exp funenv env exp2 reasons in
          debug_printf "\ninfered_arg_typ2: %s\n" (string_of_typ infered_arg_typ2);
          (* Make up a brand-new type variable for the return type of the operation. *)
          let new_return_tyvar = TyVar(gensym "prim2res", loc) in
          (* Construct a new arrow type using the inferred types of the argument(s) and the made-up return type variable. *)
          let new_op_tyarr = TyArr([infered_arg_typ1; infered_arg_typ2], new_return_tyvar, loc) in
          debug_printf "\nnew_op_tyarr: %s\n" (string_of_typ new_op_tyarr);
          (* Recursively unify the looked-up arrow type of the operator, with the constructed arrow type. *)
          let unify_subs = unify looked_up_tyarr new_op_tyarr loc reasons in
          debug_printf "\nUNIFIED SUBS\n";
          print_subst unify_subs;
          (* If all goes well, return the newly-constructed return type variable, and the substitution obtained from the recursive unification call. *)
          let all_subs = compose_subst (compose_subst subs1 unify_subs) subs2 in
          (all_subs, new_return_tyvar, e)
  | EBool(b, a) -> ([], tBool, e)
  | ENumber _ -> ([], tInt, e)
  | EId(str,loc) -> ([],find_pos env str loc, e)
  | EApp(funame, arglist, loc) ->(*
          (* Lookup the relevant scheme *)
          let looked_up_scheme = find_pos funenv funame loc in
          (* Instantiate it to a type *)
          let looked_up_tyarr = instantiate looked_up_scheme in
          (* Infer a type for the argument(s) of the primitive. *)
          let sub_infer_trips = List.map (fun e -> infer_exp funenv env e reasons) arglist in
          (* Make up a brand-new type variable for the return type of the operation. *)
          let new_return_tyvar = TyVar(gensym "app_res", loc) in
          (* Construct a new arrow type using the inferred types of the argument(s) and the made-up return type variable. *)
          let infer_lst = List.map (fun (_, ele, _) -> ele) sub_infer_trips in
          let new_op_tyarr = TyArr(infer_lst, new_return_tyvar, loc) in
          (* Recursively unify the looked-up arrow type of the operator, with the constructed arrow type. *)
          let unify_subs = unify looked_up_tyarr new_op_tyarr loc reasons in
          (* If all goes well, return the newly-constructed return type variable, and the substitution obtained from the recursive unification call. *)
          let all_subs = List.fold_left (fun acc ele -> compose_subst acc ele) unify_subs (List.map (fun (ele, _, _) -> ele) sub_infer_trips) in
          (all_subs, new_return_tyvar, e)*)
            raise (NotYetImplemented("Finish this case"))

  | EAnnot(exp, typ,loc) ->  
    let(exp_subt, exp_typ, exp)  = infer_exp funenv env exp reasons in
    let exp_type = apply_subst_typ exp_subt typ in
    (exp_subt, exp_type, e)
  | ETuple(exp_lst, loc) ->
    let infered = List.map (fun ele -> infer_exp funenv env ele reasons) exp_lst in
    let substs = (List.flatten (List.map (fun (s, _, _) -> s) infered)) in
    let this_typ = List.map (fun (_, t, _) -> t) infered in
    (substs, TyTup(this_typ, loc), e)
  | EGetItem(e, i1, i2, loc) ->
    let (subst, etyp, new_e) = infer_exp funenv env e reasons in
    (match etyp with
    | TyTup(typ_list, loc1) ->
        if (i1 > i2) then raise (InternalCompilerError (sprintf "Index out of bounds %d of %d" i1 i2))
                     else (subst, (List.nth typ_list i1), new_e)
    | _ -> raise (InternalCompilerError "infered exp for tuple to non tytup :/"))
  | ESeq _ -> raise (InternalCompilerError "Desugar fail much?")
  | ESetItem(_, _, _, _, loc) -> ([], TyVar(gensym "you_shall_not_type", loc), e)
  | ENil(t, loc) -> ([], t, e)
;;



let infer_decl funenv env (decl : sourcespan decl) reasons : (sourcespan scheme envt * sourcespan typ) =
    let DFun(funame, arg_lst, _, body, loc) = decl in
    let SForall(_, my_type, _) = find_pos funenv funame loc in
    let arg_names = List.map (fun ele ->
        (match ele with
        | BName(n, _, _) -> n
        | _ -> raise (InternalCompilerError "Arg name not bname :/"))) arg_lst in
    match my_type with
        |  TyArr(arg_typ, my_ret_typ, _) -> 
            let env = List.fold_left2
                (fun e n b -> StringMap.add n b e) env arg_names arg_typ in
            let (b_sub, bod_typ, _) = infer_exp funenv env body reasons in
            debug_printf "\n type: %s %d" (string_of_typ bod_typ) (List.length b_sub);
            print_subst b_sub;
            let unify_subst = unify bod_typ my_ret_typ loc reasons in
            debug_printf "\n UNIFY SUBST";
            print_subst unify_subst;
            let all_subst = compose_subst b_sub unify_subst in
            print_subst all_subst;
            debug_printf "\nmytype: %s" (string_of_typ my_type);
            let fin_type = apply_subst_typ all_subst my_type in
            debug_printf "\ntype: %s" (string_of_typ fin_type);
            let env = apply_subst_env all_subst env in
            print_env env;
            let fin_scheme = generalize env fin_type in            
            let fin_scheme = apply_subst_scheme all_subst fin_scheme in
            debug_printf "\nscheme: %s" (string_of_scheme fin_scheme);
            let funenv = StringMap.add funame fin_scheme funenv in
            (funenv, fin_type)
        | _ -> raise (InternalCompilerError "Fn init to something other than a tyarr :/")
;;
 
let init_fn fn =
    let DFun(funname, arg_lst, scheme, body, pos) = fn in
    let arg_lst = List.map (fun ele -> 
        (match ele with
        | BName(arg_name, _, _) -> arg_name
        | _ -> raise (InternalCompilerError "Arg name not bname :/"))) arg_lst in
    let ty_args = List.map (fun ele -> TyVar(gensym (sprintf "fn_arg_%s" ele), pos)) arg_lst in
    let ty_bod = TyVar(gensym (sprintf "%s_bod" funname), pos) in
    (funname, TyArr(ty_args, ty_bod, pos))

let infer_group funenv env (g : sourcespan decl list) : (sourcespan scheme envt * sourcespan decl list) =
    (* - Guess type variables for all functions in group. *)
    let typ_env_lst = List.map init_fn g in
    (* Add new function to type mappings to funenv *)
    let tmp_funenv = List.fold_left
        (fun e (fname, ftype) -> StringMap.add fname (generalize env ftype) e)
        funenv typ_env_lst in 
    (* Infer types for each function body, and accumulate the substitutions that result. *)
    let funenv = (List.fold_left
        (fun acc ele ->
            let (new_funenv, _) = infer_decl acc env ele [] in
            new_funenv) 
        tmp_funenv g) in
    print_funenv funenv;
    (funenv, g)
;;

let infer_prog funenv env (p : sourcespan program) : sourcespan program =
    match p with
  | Program(tydecls, declgroups, body, tag) ->
          (* Infer type on groups to build env *)
          let built_env = List.fold_left
            (fun acc ele -> let (grp_env, _) = infer_group acc env ele in grp_env)
            funenv (* Accumulate new funenv elements into old one. *)
            declgroups in
          (* Infer type on body in built env *)
          let _ = infer_exp built_env env body [] in
          (* If we get this far, looks like the types are fine. *)
          p
;;
let type_synth (p : sourcespan program) : sourcespan program fallible =
  try
    Ok(infer_prog initial_env StringMap.empty p)
  with e -> Error([e])
;;
