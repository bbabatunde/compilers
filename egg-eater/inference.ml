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
      ("add1",int2int);("sub1",int2int);("print",any2any);("isbool",any2bool);("isnum",any2bool);("not",bool2bool);
      ("plus",intint2int);("minus",intint2int);("and",boolbool2bool);("or",boolbool2bool);("greater",boolbool2bool);
      ("greaterq",boolbool2bool);("less",boolbool2bool);("lesseq",boolbool2bool);("eq",anyany2bool);("eqb",boolbool2bool);
      ("times",intint2int);("printb",any2any);
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
let ty_err t1 t2 loc reasons = TypeMismatch(loc, t2, t1, reasons)
let rec unify (t1 : 'a typ) (t2 : 'a typ) (loc : sourcespan) (reasons : reason list) : 'a typ subst =
    (match t1 with
    | TyVar(varname, pos) ->
        (match t2 with
        | TyCon(ty2, pos) -> bind varname t2
        | TyVar(ty2, pos) -> bind varname t2
        | _ -> raise (ty_err t1 t2 loc reasons))
    | TyCon(conname, pos) ->
        (match t2 with
        | TyCon(ty2, pos) -> if conname = ty2 then [] else raise (ty_err t1 t2 loc reasons)
        | TyVar(varname, pos) -> bind varname t2
        | _ -> raise (ty_err t1 t2 loc reasons))
    | TyArr(l1, ty1, pos) ->
        (match t2 with
        | TyArr(l2, ty2, pos) -> if ((List.length l1) != (List.length l2)) then raise (ty_err t1 t2 loc reasons) else
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2)) @
            (unify t1 t2 pos reasons)
        | _ -> raise (ty_err t1 t2 loc reasons))
    | TyApp(appTyp, l1, pos) ->
        (match t2 with
        | TyApp(ty2, l2, pos) ->
            (unify appTyp ty2 pos reasons) @
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2))
        | _ -> raise (ty_err t1 t2 loc reasons))
    | TyTup(l1, pos) ->
        (match t2 with
        | TyTup(l2, pos) ->
            (List.flatten (List.map2 (fun ele1 ele2 -> unify ele1 ele2 loc reasons) l1 l2))
        | _ -> raise (ty_err t1 t2 loc reasons))
    | TyBlank(pos) -> raise (ty_err t1 t2 loc reasons))

(*
  | TyBlank(pos),TyBlank(pos2) -> []
  | TyCon(str,pos),TyCon(str2,pos2) ->   if str = str2 then [] else raise (ty_err t1 t2 loc reasons) 
  | TyVar(str,pos),TyVar(str2,pos2) ->   bind str t2 
  | TyArr(typlist, typ,pos),TyArr(typlist2,typ2, pos2) -> if List.length typlist = List.length typlist2
                                                          then (List.flatten (List.map2 (fun e1 e2  ->  unify e1 e2 loc reasons) typlist typlist2)) @ 
                                                          (unify typ typ2 loc reasons) 
                                                          else raise (ty_err  t1 t2 loc reasons) 
  | TyApp(typ,typlist,pos), TyApp(typ2, typlist2,pos2)->  failwith "implement unify for TyApp" 
  |_ -> raise (ty_err t1 t2 loc reasons) 
*)
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
 |SForall(strlst, typ,pos) -> TyArr( (List.fold_left (fun lst t -> lst @ [(TyCon(gensym t,dummy_span))] ) [] strlst), unblank typ, pos)
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
                  let (n, t, _) =  (match btype with
                    | BName(n, t, l) -> (n, t, l)
                    | _ -> raise (InternalCompilerError "Binding of non variable name found :/ Desugar much?")) in
                  (* Process expr and get subs *)
                  let (e_sub, e_type, _) = infer_exp funenv env e reasons in
                  (* Throw out the annotated type, should be blank. *)
                  (* Add new computed binding to env *)
                  let new_env = StringMap.add n e_type env in
                  (new_env, e_sub))
              (env, []) binds in
          (* infer type on body to get result type *)
          let (exp_subst, exp_type, _) = infer_exp funenv new_env exp reasons in
          (* resulting substitution is final substitution *)
          let final_subst = compose_subst new_subst exp_subst in
          (* see if constraints have a solution *)
          let exp_type = apply_subst_typ final_subst exp_type in
          (final_subst, exp_type, e)
  | EPrim1(op, exp,loc) ->  
          let (subs, exp_typ, _) = infer_exp funenv env exp reasons in
          let env = apply_subst_env subs env in
          (match op with
            | Add1 -> let new_subs = unify exp_typ tInt loc reasons in (new_subs, (apply_subst_typ new_subs exp_typ), e)
            | Sub1 -> ([], TyArr([tInt], tInt, loc), e)
            | Print -> 
              
              let typ_scheme = find_pos funenv "print" loc in
              let instantiate_scheme = instantiate typ_scheme in
              let (exp_sub, exp_typ, exp) = infer_exp funenv env exp reasons in

              let new_typevar = TyCon(gensym "printresult", loc) in

              let add1_arrowtype = TyArr([exp_typ] , new_typevar, loc) in
              let unif_sub1 = unify instantiate_scheme add1_arrowtype loc reasons in
              (unif_sub1, add1_arrowtype, e)

            | IsBool  
            | IsNum 
            | Not -> ([], TyArr([tBool], tBool, loc), e)
            | PrintStack ->  failwith "Finish implementing inferring types for PrintStack")
  | EPrim2(op, l,r,loc) -> begin match op with
      | Plus -> 

             let typ_scheme =  find_pos funenv "plus" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tInt in
             let l_typ = apply_subst_typ subst_so_far tInt in
        

             let new_typevar = TyCon(gensym "plusresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)

      | Minus -> 
             let typ_scheme =  find_pos funenv "minus" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tInt in
             let l_typ = apply_subst_typ subst_so_far tInt in
        

             let new_typevar = TyCon(gensym "minusresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)

      | Times -> 
             let typ_scheme =  find_pos funenv "times" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tInt in
             let l_typ = apply_subst_typ subst_so_far tInt in
        

             let new_typevar = TyCon(gensym "timesresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)

      | And -> 
             let typ_scheme =  find_pos funenv "and" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "andresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | Or -> 
             let typ_scheme =  find_pos funenv "or" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "orresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | Greater -> 
             let typ_scheme =  find_pos funenv "greater" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "greaterresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | GreaterEq -> 
             let typ_scheme =  find_pos funenv "greatereq" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "greatereqresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | Less -> 
             let typ_scheme =  find_pos funenv "less" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "lessreqresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | LessEq -> 
             let typ_scheme =  find_pos funenv "lesseq" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "lesseqreqresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
      | Eq -> 
             let typ_scheme =  find_pos funenv "eq" loc in 
             let instantiate_scheme = instantiate typ_scheme in
             let (l_subst, l_typ, l) = infer_exp funenv env l reasons in
             let env = apply_subst_env l_subst env in 
             let (r_subst, r_typ, r) = infer_exp funenv env r reasons in
              
             let subst_so_far = compose_subst l_subst r_subst  in 

             let r_typ = apply_subst_typ subst_so_far tBool in
             let l_typ = apply_subst_typ subst_so_far tBool in
        

             let new_typevar = TyCon(gensym "eqresult", loc) in 

             let plus1_arrowtype = TyArr([l_typ;r_typ], new_typevar, loc) in 

             let unifyresult = unify  instantiate_scheme plus1_arrowtype loc reasons in 
             let final_subst = compose_subst unifyresult subst_so_far in 
             (final_subst, plus1_arrowtype, e)
              end
  | EBool(b, a) -> ([], tBool, e)
  | ENumber _ -> ([], tInt, e)
  | EId(str,loc) -> ([],find_pos env str loc, e)
  | EApp(funame, arglist,loc) -> failwith "EApp decl."
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
;;



let infer_decl funenv env (decl : sourcespan decl) reasons : sourcespan scheme envt * sourcespan typ subst =
    (* Collect all the free type variables in the type of the function body *)
    let DFun(name, args, _, body, loc) = decl in
    (* Scheme for this funciton must be in funenev *)
    let SForall(_, typ_scheme, _) = find_pos funenv name loc in
    let TyArr(tmp_args, tmp_type, _) = typ_scheme in
    let (body_subst, body_type, _) = infer_exp funenv env body reasons in
    let body_type = apply_subst_typ body_subst tmp_type in
    let unif_subst = unify tmp_type body_type loc reasons in
    let final_subst = compose_subst body_subst unif_subst in
    let final_type = apply_subst_typ final_subst tmp_type in
    let funenv = apply_subst_funenv final_subst funenv in
    ((StringMap.add name (generalize env final_type) funenv), body_subst)
;;
 
let init_fn fn =
    let DFun(funname, arg_lst, scheme, body, pos) = fn in
    let arg_lst = List.map (fun ele -> 
        (match ele with
        | BName(arg_name, _, _) -> arg_name
        | _ -> raise (InternalCompilerError "Arg name not bname :/"))) arg_lst in
    let ty_args = List.map (fun ele -> TyVar(gensym "fn_arg", pos)) arg_lst in
    let new_env = (List.fold_left2
        (fun ne arg_name arg_type -> (StringMap.add arg_name arg_type ne))
        StringMap.empty arg_lst ty_args) in
    let ty_bod = TyVar(gensym "fn_bod", pos) in
    (funname, TyArr(ty_args, ty_bod, pos), new_env)

let infer_group funenv env (g : sourcespan decl list) : (sourcespan scheme envt * sourcespan decl list) =
    (* Instantiate the type schemes for the functions all at once. *)
    (* - Guess type variables for all functions in group. *)
    let typ_env_lst = List.map init_fn g in
    (* - Bind args to these type variables in env to process decls in. *)
    let env = List.fold_left
        (fun e (_, _, n_map) -> 
            List.fold_left
            (fun e (k, d) -> StringMap.add k d e) e (StringMap.bindings n_map))
        env typ_env_lst in
    (* Add new function to type mappings to funenv *)
    let funenv = List.fold_left
        (fun e (fname, ftype, _) -> StringMap.add fname (generalize env ftype) e)
        funenv typ_env_lst in
    (* Infer types for each function body, and accumulate the substitutions that result. *)
    let new_types_and_subs = (List.map
        (fun fn -> infer_decl funenv env fn []) g) in
    (* Generalize all the remaining types all at once. *)
    let funenv = List.fold_left
        (fun e (n_map, _) ->
            List.fold_left
            (fun e (k, d) -> StringMap.add k d e) e (StringMap.bindings n_map))
        funenv new_types_and_subs in
    (* Combine the substs *)
    let allsubst = List.fold_left
        (fun acc (_, s) ->
            (acc @ s)) [] new_types_and_subs in
    (* Check for a solution *)
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
          let _ = infer_exp built_env env body []
          (* If we get this far, looks like the types are fine. *)
          in p
;;
let type_synth (p : sourcespan program) : sourcespan program fallible =
  try
    Ok(infer_prog initial_env StringMap.empty p)
  with e -> Error([e])
;;
