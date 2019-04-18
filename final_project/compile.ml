open Printf
open Exprs
open Assembly
open Errors
open Pretty
open Phases


type 'a envt = (string * 'a) list
module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

let skip_typechecking = ref false
let show_debug_print = ref true

let debug_printf fmt =
    if !show_debug_print
    then printf fmt
    else ifprintf stdout fmt
;;

     
let gensym =
  let count = ref 0 in
  let next () =
    count := !count + 1;
    !count
  in fun str -> sprintf "%s_%d" str (next ());;


let find env x loc =
  let rec help ls =
    match ls with
    | [] -> raise (InternalCompilerError (sprintf "Name %s not found at %s" x loc))
    | (y,v)::rest ->
       if y = x then v else help rest
  in help env
let const_true = HexConst (0xFFFFFFFF)
let const_false = HexConst(0x7FFFFFFF)
let bool_mask = HexConst(0x80000000)
let tag_as_bool = HexConst(0x00000001)

let err_COMP_NOT_NUM   = 0
let err_ARITH_NOT_NUM  = 1
let err_LOGIC_NOT_BOOL = 2
let err_IF_NOT_BOOL    = 3
let err_OVERFLOW       = 4

let err_COMP_NOT_NUM   = 1
let err_ARITH_NOT_NUM  = 2
let err_LOGIC_NOT_BOOL = 3
let err_IF_NOT_BOOL    = 4
let err_OVERFLOW       = 5
let err_GET_NOT_TUPLE  = 6
let err_GET_LOW_INDEX  = 7
let err_GET_HIGH_INDEX = 8
let err_INDEX_NOT_NUM  = 9
let error_not_closure = 10
let error_wrong_arity  = 11

                             


let fun_prim = ["input";"print"]

let rec strip_binds arg =
  let result = (List.fold_left (fun env b -> match b with 
  |BBlank(_,_) -> env
  |BName( str, typ , loc) ->  [(str , loc)]@env) [] arg)
  in result

let rec findlst ls str = 
   match ls with
  |[] -> None
  |TyDecl(x,_,_)::rest  -> if x = str then  Some(x) else findlst rest str 

let rec remove_one_str ls str =
  match ls with
  |[] -> ls
  |TyDecl(x,_,_)::rest -> if x == str then  rest else remove_one_str rest str 

 let rec findecls decls str =
 match decls with 
 |[] -> None
 |TyDecl(name,args,loc)::rest -> if name = str then Some(TyDecl(name,args,loc)) else findecls rest str



(* You may find some of these helpers useful *)
let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y,v)::rest ->
     if y = x then v else find rest x


let rec find2 ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
     if y = x then Some(v) else find2 rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq(e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ALetRec(binds, body, _) ->
       (List.length binds) + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
    | ANewObject _ -> 0
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in helpA e

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))


let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name
    | _::ds_rest -> find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs
;;

let remove_one_decl (ls : 'a decl list) (n : string)  : 'a decl list = 
  let rec find_decl2 (ds : 'a decl list) (name : string) : 'a decl list option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _, _,_))::ds_rest ->
      if name = fname then Some(ds_rest) else find_decl2 ds_rest name
    | _::ds_rest -> find_decl2 ds_rest name
  in 
  match (find_decl2  ls n) with
  |None -> ls
  |Some(e) -> e


let remove_one_arg (ls : (string * sourcespan) list) (elt : string) : (string * sourcespan) list = 
  let rec find_one2 (l : (string * sourcespan) list) (elt : string) : (string * sourcespan) list option =
  match l with
    | [] -> None
    | (x,y)::xs -> if (elt = x)  then Some(xs) else (find_one2 xs elt)
  in 
  match (find_one2  ls elt) with
  |None -> ls
  |Some(e) -> e

;;

let rec bindsToBindings bl =
    match bl with
    | first::rest ->
        (match first with
        | BName(n, _, l) -> (first, EId(n, l), l)::(bindsToBindings rest)
        | _ -> raise (InternalCompilerError("Binds to bindings conversion is only for Bname.")))
    | [] -> []
;;


type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let rec bindsToStrings binds =
    match binds with
    | first::rest ->
        (match first with
        | BName(n, _, _) -> n :: (bindsToStrings rest)
        | _ -> raise (InternalCompilerError("Lamdas must take only Bnames after desugar.")))
    | [] -> []
;;

let rec bindingsToStrings binds =
    match binds with
    | (first, _, _)::rest ->
        (match first with
        | BName(n, _, _) -> n :: (bindingsToStrings rest)
        | _ -> raise (InternalCompilerError("BLetRecs need to be BNames!")))
    | [] -> []
;;
                             
(*
let is_well_formed (p : sourcespan program)   : (sourcespan program) fallible =
  let rec wf_E (e: sourcespan expr) (ds : 'a decl list) (env : (string * sourcespan) list) (tydecls: 'a tydecl list) 
  : exn list = match e with
    | ENumber(n, pos) -> if n > 1073741823 || n < -1073741824 then
       [Overflow(n,pos)] else []
    | EBool(_, _) -> []
    | EId(name, pos) -> 
      begin match find2 env name with
      |None -> [UnboundId(name,pos)]
      |Some(d) -> [] end
    | EAnnot(exp, _, _) -> wf_E exp ds env tydecls
    | EPrim1(op, arg, _) ->  wf_E arg ds env tydecls
    | EPrim2(op, left, right, _) ->  wf_E left ds env tydecls @ wf_E right ds env tydecls
    | EIf(cond, _then, _else, _) -> wf_E cond ds env tydecls @ wf_E _then ds env tydecls @ wf_E _else ds env tydecls
    | EApp(funname, appargs, pos) -> List.fold_left (fun lst e -> wf_E e ds env tydecls) [] appargs
    | ELambda(bindl, e, pos) ->
            let dups = (check_arg_duplicate (bindsToStrings bindl) pos) in
            if List.length dups > 0 then dups else (wf_E e ds (add_bindlst_env bindl  env) tydecls)
    | ELetRec(bindl, e, pos) ->
            let dups = (check_bind_duplicate (bindingsToStrings bindl) pos) in
            if List.length dups > 0 then dups else
                let dups = (check_binding_duplicate bindl ds env tydecls) in
                if List.length dups > 0 then dups else check_lamb_body bindl @ wf_E e ds (add_bindlst_env (bindingsToBinds bindl) env) tydecls
    |ELet([], body, pos) -> wf_E body ds env tydecls
    |ELet((bind,expr,exploc)::rest as bindings, body, pos) -> 
     let shadowlist = begin match bind with
                     | BBlank(typ,loc) -> wf_T typ tydecls
                     | BName(name, typ,loc) ->  (check_shadowid name  expr exploc env ds tydecls) @ wf_T typ tydecls end
    in
    let(exnbinds_list,newenv) = check_binding_list bindings env ds tydecls in 
    (shadowlist @ exnbinds_list @ (wf_E body ds newenv tydecls))  

   | ESeq(left, right,_) -> wf_E left ds env  tydecls @ wf_E right ds env tydecls
   | ENil(typ,_) ->  wf_T typ  tydecls
   | _ -> failwith "WF not implemented for these!!"

and bindingsToBinds bl =
    match bl with
    | (b, _, _)::rest ->
         b :: (bindingsToBinds rest)
    | [] -> []

and check_binding_duplicate bindl ds env tds=
 (match bindl with
    | [] -> []
    | (_, arg, _)::rest -> (wf_E arg ds env tds) @ (check_binding_duplicate rest ds env tds))

and check_lamb_body bindl =
    (match bindl with
    |(BName(n, t, p), e, l)::rest -> (match e with
        | ELambda _ -> check_lamb_body rest
        | _ -> [LetRecNonFunction(e, l)] @ check_lamb_body rest)
    | [] -> [] 
    | _ -> raise (InternalCompilerError "Parser broken? Non name in ELetRec binding.")
    )

and check_bind_duplicate bindl pos =
  (match bindl with
        | [] -> []
        | first::rest -> if List.mem first rest then ([DuplicateLetRecDecl(first, pos)] @ check_bind_duplicate rest pos) else check_bind_duplicate rest pos)

and check_arg_duplicate bindl pos =
  (match bindl with
        | [] -> []
        | f::r -> if List.mem f r then ([DuplicateArgument(f, pos)] @ check_arg_duplicate r pos) else check_arg_duplicate r pos)

and check_bind_list bind_list expr exproc env ds tydecls: exn list = 
  List.fold_left (fun exnlst b -> exnlst @  check_shadowid_tuple b expr exproc env ds tydecls) [] bind_list

and check_shadowid_tuple b expr exploc env ds tydecls: exn list = match b with
  | BBlank(typ,_) ->  []
  | BName(str, typ,loc) -> (check_shadowid str  expr exploc env ds tydecls) 


and check_shadowid bind expr exploc env ds tydecls: exn list = match find2 env bind with
  |None -> wf_E expr ds env tydecls
  |Some(loc) -> [ShadowId(bind,loc,exploc)]

and check_binding_list (bindings: 'a binding list) (env : (string * sourcespan) list) (ds : 'a decl list) (tydecls: 'a tydecl list) : 
  ((exn list )  * ((string * sourcespan) list))=
   (List.fold_left (fun  (exnlist,env) (b: 'a binding) -> match b with 
       |(BBlank(typ, _),    e,   y) ->  (wf_E e ds env tydecls, env)
       |(BName(n,typ,loc),  e,   y)  -> match find2 env n with  
                                          |None ->(exnlist, [(n,y)]@env)
                                          |Some(exploc) ->((wf_E e ds ([(n,y)] @ env) tydecls) @[DuplicateId(n,exploc,y)]@exnlist , [(n,y)]@env))

   ([],env) bindings)
 
and add_bindlst_env blst env = 
 List.fold_left (fun env e -> 
    match e with
   | BBlank(typ,_) ->  env
   | BName(str, typ,loc) -> [(str,loc)]@env
    )  env blst

  and wf_D  (ds : 'a decl list) (tydecls: 'a tydecl list): exn list = 
    let result = 
    (List.fold_left (fun errorlst (DFun(funname,  ars,sch, body, upos)) ->
      let args = strip_binds ars in 
      let dupfunlist = match (find_decl (remove_one_decl ds funname) funname)  with
       |None -> errorlst@(wf_E body ds args tydecls) @ (wf_S sch  tydecls)
       |Some(DFun(name, _,_, _body, dpos)) -> errorlst@[DuplicateFun(funname,upos,dpos)]
         @(wf_E body ds args tydecls)
         @ (wf_S sch tydecls)
      and dupargslist = (List.fold_left (fun exnlist (arg,argloc) -> match (find2 (remove_one_arg args arg) arg) with
            |None -> errorlst
            |Some(loc) -> errorlst@[DuplicateId(arg,loc,argloc)]
          ) [] args) in (dupfunlist @ dupargslist)) [] ds) in result 
  and wf_G (gds : 'a decl list list ) (tydecls: 'a tydecl list): exn list  =
   let (has_seen, exnlist) = (List.fold_left (fun (has_seen, exnlist) gd -> (gd@has_seen, exnlist@(wf_D (gd@has_seen) tydecls)))  ([],[]) gds)
     in exnlist
  (*TODO check for type intlist = (intlist * int)*)
   and wf_T (t : sourcespan typ) (tydecls: 'a tydecl list)  : exn list = match t with 
    | TyBlank(_) -> []
    | TyCon("Int", _) -> []
    | TyCon("Bool",_)-> []
    | TyCon(str,loc) -> begin match (findlst  tydecls  str) with 
                          |None -> [Unsupported(str,loc)]
                          |Some(x) -> [] end
    | TyVar(name,loc)->  begin match findecls tydecls name with
                        |None -> [InvalidTyLen(name, loc)]
                        |Some(x) -> [] end
    | TyArr(typlist,typ,_)-> []
    | TyApp(typ,typlist,_) -> []
  and wf_S (s : sourcespan scheme)  (tydecls: 'a tydecl list): exn list = match s with 
    | SForall(strlst,typ,loc) ->   (wf_T typ  tydecls)

  and wf_TD (tlst : sourcespan tydecl list) : exn list = 
    let result = ( List.fold_left (fun error t ->   match t with 
        |TyDecl(name,args,loc) ->   match (findlst (remove_one_str tlst name) name) with
        |None -> (error @  (List.fold_left (fun error t -> error @ wf_T t tlst) [] args) @ 
         (if List.length args > 2 then [InvalidTyLen(name,loc)] else [])  @
         (if List.hd args = List.hd (List.rev args)  then [CyclicTy(name,loc)] else []))
        |Some(x) -> error @ [DuplicateType(name,loc)] @  (List.fold_left (fun error t -> error @ wf_T t tlst) [] args))  [] tlst) 
       in result
  in
  match p with
  | Program(tydecls, decls, body, _) ->
     let output = wf_TD tydecls @  wf_G decls tydecls @ wf_E body (List.flatten decls)  [] tydecls in
     if output = [] then Ok(p) else Error(output)
;;
*)

let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program(tydecls, decls, body, tag) ->
       Program(tydecls, List.map (fun g -> List.map (helpD env) g) decls, helpE env body, tag)
  and helpD env decl =
    match decl with
    | DFun(name, args, scheme, body, tag) ->
       let (newArgs, env') = helpBS env args in
       DFun(name, newArgs, scheme, helpE env' body, tag)
    | DClass(n, b, d, tag) ->
       let (revbinds, env) = List.fold_left (fun (revbinds, env) (b, e, t) ->
                                let (b, env) = helpB env b in ((b, e, t)::revbinds, env)) ([], env) b in
       let bindings' = List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag)::bindings) [] revbinds in
       let new_decls = List.map (helpD []) d in
       DClass(n, bindings', new_decls, tag)
    | DClassE(n, b, d, e, tag) ->
        let (revbinds, env) = List.fold_left (fun (revbinds, env) (b, e, t) ->
                                let (b, env) = helpB env b in ((b, e, t)::revbinds, env)) ([], env) b in
       let bindings' = List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag)::bindings) [] revbinds in
       let new_decls = List.map (helpD []) d in
       DClassE(n, bindings', new_decls, e, tag)
  and helpB env b =
    match b with
    | BBlank(typ, tag) -> (b, env)
    | BName(name, typ, tag) ->
       let name' = sprintf "%s_%d" name tag in
       (BName(name', typ, tag), (name, name') :: env)
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b::bs ->
       let (b', env') = helpB env b in
       let (bs', env'') = helpBS env' bs in
       (b'::bs', env'')
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a)::bindings ->
       let (b', env') = helpB env b in
       let e' = helpE env' e in
       let (bindings', env'') = helpBG env' bindings in
       ((b', e', a)::bindings', env'')
  and helpE env e =
    match e with
    | EAnnot(e, t, tag) -> helpE env e
    | ESeq(e1, e2, tag) -> ESeq(helpE env e1, helpE env e2, tag)
    | EPrim1(op, arg, tag) -> EPrim1(op, helpE env arg, tag)
    | EPrim2(op, left, right, tag) -> EPrim2(op, helpE env left, helpE env right, tag)
    | EIf(c, t, f, tag) -> EIf(helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId(name, tag) ->
       (try
         EId(find env name, tag)
       with Not_found -> e)
    | EApp(name, args, tag) -> EApp(name, List.map (helpE env) args, tag)
    | ELet(binds, body, tag) ->
       let (binds', env') = helpBG env binds in
       let body' = helpE env' body in
       ELet(binds', body', tag)
    | ELetRec(bindings, body, tag) ->
       let (revbinds, env) = List.fold_left (fun (revbinds, env) (b, e, t) ->
                                let (b, env) = helpB env b in ((b, e, t)::revbinds, env)) ([], env) bindings in
       let bindings' = List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag)::bindings) [] revbinds in
       let body' = helpE env body in
       ELetRec(bindings', body', tag)
    | ELambda(binds, body, tag) ->
       let (binds', env') = helpBS env binds in
       let body' = helpE env' body in
       ELambda(binds', body', tag)
    | ENewObject _ -> e
    | EObject _ -> e
    | EMethodCall(name, meth, args, n2, tag) ->
       EMethodCall((helpE env name),  meth, List.map (helpE env) args, n2, tag)
    | ESetField(obj, field, new_val, obj_name, tag) ->
       ESetField((helpE env obj), field, (helpE env new_val), obj_name, tag)
    | EGetField(obj, field, objclass, tag) ->
       EGetField((helpE env obj), field, objclass, tag)
  in (rename [] p)
;;


(*
let defn_to_letrec (p : 'a program) : 'a program =
  let rec wrap groups body tag =
    match groups with
    | [] -> body
    | group::rest -> wrap_group group (wrap rest body tag) tag
  and wrap_group decls body tag =
    let decl_to_binding d =
      match d with
      | DFun(name, args, scheme, body, tag) ->
         (BName(name, instantiate scheme, tag), ELambda(args, body, tag), tag) in
    ELetRec(List.map decl_to_binding decls, body, tag) in
  match p with
  | Program(tydecls, declgroups, body, tag) ->
     Program(tydecls, [], wrap declgroups body tag, tag)*)


let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program(_, decls, body, _) -> AProgram(List.concat(List.map helpG decls), helpA body, ())
  and helpG (g : tag decl list) : unit adecl list =
    List.map helpD g
  and helpD (d : tag decl) : unit adecl =
    match d with
    | DFun(name, args, ret, body, _) ->
       let args = List.map (fun a ->
                      match a with
                      | BName(a, _, _) -> a
                      | _ -> raise (InternalCompilerError("Tuple bindings should have been desugared away"))) args in
       ADFun(name, args, helpA body, ())
    | DClass(name, fields, methods, _) ->
        let new_fields = List.map (fun a ->
            match a with
            | (BName(name, _, _), default, _) -> 
                    let (def_imm, def_setup) = helpI default in
                    if List.length def_setup != 0 then raise (InternalCompilerError("DClass immediate not non context immediate :/")) else
                    (name, def_imm)
            | _ -> raise (InternalCompilerError("BBlank as a class name? o.O"))
            ) fields in
        let new_methods = List.map helpD methods in
        AClass(name, new_fields, new_methods, ())
    | DClassE(name, fields, methods, extends, _) ->
        let new_fields = List.map (fun a ->
            match a with
            | (BName(name, _, _), default, _) -> 
                    let (def_imm, def_setup) = helpI default in
                    if List.length def_setup != 0 then raise (InternalCompilerError("DClass immediate not non context immediate :/")) else
                    (name, def_imm)
            | _ -> raise (InternalCompilerError("BBlank as a class name? o.O"))
            ) fields in
        let new_methods = List.map helpD methods in
        AClassE(name, new_fields, new_methods, extends, ())
   and helpC (e : tag expr) : (unit cexpr * unit anf_bind list) = 
    match e with
    | EAnnot(e, _, _) -> helpC e
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (CIf(cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet([], body, _) -> helpC body
    | ELet((BBlank(_, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BSeq(exp_ans)] @ body_setup)
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup)
    | EApp(funname, args, _) ->
       let (fun_ans, fun_setup) = helpI funname in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(fun_ans, new_args, ()), fun_setup @ List.concat new_setup)

    | ENewObject(name, _) -> (CNewObject(name, ()), [])

    | ESeq(e1, e2, _) ->
       let (e1_ans, e1_setup) = helpC e1 in
       let (e2_ans, e2_setup) = helpC e2 in
       (e2_ans, e1_setup @ [BSeq e1_ans] @ e2_setup)
    | ELambda(binds, body, _) ->
       let args = List.map (fun a ->
                      match a with
                      | BName(a, _, _) -> a
                      | BBlank(_, _) -> gensym "blank")
                    binds in
       (CLambda(args, helpA body, ()), [])
    | ELetRec(binds, body, _) ->
       let name_of b =
         match b with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError "Other bindings should be desugared or rejected") in
       let (names, new_binds_setup) = List.split (List.map (fun (b, rhs, _) -> (name_of b, helpC rhs)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (body_ans, (BLetRec (List.combine names new_binds)) :: body_setup)

    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * unit anf_bind list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | ENil _ -> (ImmNil(), [])
    | ENewObject(name, tag) ->
        let tmp = sprintf "new_obj_%d" tag in
        let (newo_imm, newo_setup) = helpC e in
        (ImmId(tmp, ()), newo_setup @ [BLet(tmp, newo_imm)])
    | EObject(name, _) -> (ImmObj(name, ()), [])
    | EAnnot(e, _, _) -> helpI e
    | ESeq(e1, e2, _) ->
       let (e1_imm, e1_setup) = helpI e1 in
       let (e2_imm, e2_setup) = helpI e2 in
       (e2_imm, e1_setup @ e2_setup)

    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [BLet(tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [BLet(tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [BLet(tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(funname, args, tag) ->
       let tmp = sprintf "app_%d" tag in
       let (fun_ans, fun_setup) = helpI funname in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), fun_setup @ (List.concat new_setup) @ [BLet(tmp, CApp(fun_ans, new_args, ()))])
    | ELet([], body, _) -> helpI body
    | ELet((BBlank(_, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpI exp in (* MUST BE helpI, to avoid any missing final steps *)
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ body_setup)
    | ELambda(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let args = List.map (fun a ->
                      match a with
                      | BName(a, _, _) -> a
                      | BBlank(_, _) -> gensym "blank")
                    binds in
       (ImmId(tmp, ()), [BLet(tmp, CLambda(args, helpA body, ()))])
    | ELet((BName(bind, _, _), exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [BLet(bind, exp_ans)] @ body_setup)
    | ELetRec(binds, body, tag) ->
       let tmp = sprintf "lam_%d" tag in
       let name_of b =
         match b with
         | BName(name, _, _) -> name
         | _ -> raise (InternalCompilerError "Other bindings should be desugared or rejected") in
       let (names, new_binds_setup) = List.split (List.map (fun (b, rhs, _) -> (name_of b, helpC rhs)) binds) in
       let (new_binds, new_setup) = List.split new_binds_setup in
       let (body_ans, body_setup) = helpC body in
       (ImmId(tmp, ()), (List.concat new_setup)
                        @ [BLetRec (List.combine names new_binds)]
                        @ body_setup
                        @ [BLet(tmp, body_ans)])
    | EMethodCall(name, meth, args, n2, tag) ->
        let tmp = sprintf "emc_%d" tag in
        (* Figure out object *)
        let (name, n_setup) = helpI name in
        (* Args to the funciton call*)
        let (new_args, new_setup) = List.split (List.map helpI args) in
        (ImmId(tmp, ()),
            n_setup @
            (List.concat new_setup) @
            [BLet(tmp, CMethodCall(name, meth, new_args, getClass name, ()))])
    | ESetField(e1, s1, e2, s2, tag) ->
        let tmp = sprintf "esf_%d" tag in
        let (e1_new, e1_setup) = helpI e1 in
        let (e2_new, e2_setup) = helpI e2 in
        (ImmId(tmp, ()),
            e1_setup @
            e2_setup @
            [BLet(tmp, CSetField(e1_new, s1, e2_new, s2, ()))])
    | EGetField(e1, s1, s2, tag) ->
        let tmp = sprintf "egf_%d" tag in
        let (e1_new, e1_setup) = helpI e1 in
        (ImmId(tmp, ()),
            e1_setup @
            [BLet(tmp, CGetField(e1_new, s1, s2, ()))])
  and getClass immNewObj =
      match immNewObj with
      | ImmObj(name, _) -> name
      | _ -> raise (InternalCompilerError "Weird imm obj :/")
  and helpA e : unit aexpr = 
    let (ans, ans_setup) = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq(exp) -> ASeq(exp, body, ())
        | BLet(var_name, exp) -> 
            (match exp with
            | CNewObject(class_name, _) -> ANewObject(var_name, ImmObj(class_name, ()), class_name, body, ())
            | _ -> ALet(var_name, exp, body, ()))
        | BLetRec(names) -> ALetRec(names, body, ()))
      ans_setup (ACExpr ans)
  in
  let anf_p = helpP p in
     debug_printf "BEFORE ANF: %s\n" (string_of_program p);
     debug_printf "AFTER ANF: %s\n" (string_of_aprogram anf_p);
     anf_p
;;




let free_vars_E (e : 'a aexpr) rec_binds : string list =
  let rec helpA (bound : string list) (e : 'a aexpr) : string list =
    match e with
    | ASeq(e1, e2, _) -> helpC bound e1 @ helpA bound e2
    | ALet(name, binding, body, _) ->
     (helpC bound binding) (* all the free variables in the binding, plus *)
     (* all the free variables in the body, except the name itself *)
     @ (helpA (name :: bound) body)
    | ALetRec(bindings, body, _) ->
       let names = List.map fst bindings in
       let new_bound = (names @ bound) in
        (helpA new_bound body) @ List.flatten (List.map (fun binding -> helpC new_bound (snd binding)) bindings)
    | ACExpr c -> helpC bound c
    | ANewObject _ -> failwith "Implement this."
  and helpC (bound : string list) (e : 'a cexpr) : string list =
    match e with
    | CLambda(args, body, _) ->
      helpA (args @ bound) body
    | CIf(cond, thn, els, _) ->
      helpI bound cond @ helpA bound thn @ helpA bound els
    | CPrim1(_, arg, _) -> helpI bound arg
    | CPrim2(_, left, right, _) -> helpI bound left @ helpI bound right
    | CApp(fn, args, _) ->
      (helpI bound fn) @ (List.flatten (List.map (fun arg -> helpI bound arg) args))
    | CImmExpr i -> helpI bound i
    | CNewObject _ | CMethodCall _ | CSetField _ | CGetField _ -> failwith "Implement this."
  and helpI (bound : string list) (e : 'a immexpr) : string list =
    match e with
    | ImmId(name, _) ->
      (* a name is free if it is not bound *)
      if List.mem name bound then [] else [name]
    | _ -> []
  in List.sort_uniq String.compare (helpA rec_binds e)
;;
let free_vars_P (p : 'a aprogram) rec_binds : string list =
  match p with
  | AProgram(_, body, _) -> free_vars_E body rec_binds
;;



let rec free_typ_tyvars typ =
  match typ with
  | TyBlank _ -> []
  | TyCon _ -> []
  | TyVar(s, _) -> [s]
  | TyArr(args, ret, _) -> List.concat (List.map free_typ_tyvars (args @ [ret]))
  | TyApp(typ, args, _) -> List.concat (List.map free_typ_tyvars (args @ [typ]))
and free_scheme_tyvars (args, typ) =
  List.fold_left ExtList.List.remove (List.sort_uniq String.compare (free_typ_tyvars typ)) args
;;

let desugarPost (p : sourcespan program) : sourcespan program =
    let rec helpE e =
        (match e with
        | ESeq(e1, e2, loc) ->
            let e1_desugar = helpE e1 in
            let seq_blank = (BBlank(TyBlank(loc), loc), e1_desugar, loc) in
            ELet([seq_blank], helpE e2, loc)
        | ELet(bindings, e, loc) -> ELet(helpB bindings, helpE e, loc)
        | EPrim1(p1, e, loc) -> EPrim1(p1, helpE e, loc)
        | EPrim2(p2, e1, e2, loc) -> EPrim2(p2, helpE e1, helpE e2, loc)
        | EIf(c, t, e, loc) -> EIf(helpE c, helpE t, helpE e, loc)
        | EApp(n, el, loc) -> EApp(n, List.map helpE el, loc)
        | EAnnot(e, t, loc) -> EAnnot(helpE e, t, loc)
        | ELetRec(bindl, e, loc) -> ELetRec(helpB bindl, helpE e, loc)
        | ELambda(bindl, e, loc) -> ELambda(bindl, helpE e, loc)
        | ENewObject _ | EMethodCall _ | EObject _ | ESetField _ | EGetField _
        | ENumber _ | EBool _ | ENil _ | EId _ -> e )
    and helpB bindings =
        match bindings with
        | [] -> []
        | (b, e, l)::rest -> (b, helpE e, l) :: helpB rest
    and helpC classes =
        match classes with
        | [] -> []
        | (DClass(n, b, d, p)::[])::rest ->  DClass(n, b, d, p)::(helpC rest)
        | (DClassE(n, b, d, e, p)::[])::rest -> DClassE(n, b, d, e, p)::(helpC rest)
        | _ -> raise (InternalCompilerError("declarations not desugared right! Non class decl found."))
    in
    match p with
    | Program(tydecls, classes, body, t) ->
            let only_classes = helpC classes in
            let new_p = Program(tydecls, [only_classes], helpE body, t) in
            debug_printf "BEFORE DESUGAR_POST: %s\n" (string_of_program p);
            debug_printf "AFTER DESUGAR_POST: %s\n" (string_of_program new_p);
            new_p
    | _ -> raise (InternalCompilerError("Program function definitons not desugared into body :/"))
;;

let rec makeBlanks rem pos =
    if rem = 0 then []
    else [TyBlank(pos)] @ (makeBlanks (rem - 1) pos)
;;

let desugarPre (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    (fun name ->
        next := !next + 1;
        sprintf "%s_%d" name (!next)) in
  let rec helpE (e : sourcespan expr) (* other parameters may be needed here *) =
    match e with
    | ESeq(e1, e2, loc) ->
        ESeq(helpE e1, helpE e2, loc)
    | ELet(bindings, e, loc) -> ELet(bindings, helpE e, loc)
    | EPrim1(p1, e, loc) -> EPrim1(p1, helpE e, loc)
    | EPrim2(p2, e1, e2, loc) -> EPrim2(p2, helpE e1, helpE e2, loc)
    | EIf(c, t, e, loc) -> EIf(helpE c, helpE t, helpE e, loc)
    | EApp(n, el, loc) -> EApp(n, List.map helpE el, loc)
    | EAnnot(e, t, loc) -> EAnnot(helpE e, t, loc)
    | ELetRec(bindl, e, loc) -> ELetRec(helpB bindl, helpE e, loc)
    | ELambda(bindl, e, loc) -> ELambda(bindl, helpE e, loc)
    | ENewObject _ | EObject _  | ESetField _ | EGetField _ -> e
    | EMethodCall(obj, meth, args, _, loc) ->
        let tmp = gensym "new_obj" in
        (match obj with
        | EId(str, pos) -> EMethodCall(EObject(str, pos), meth, args, "fix", loc)
        | ENewObject(str, pos) -> e
        | _ -> raise (InternalCompilerError "Non object/id in method call."))
    | ENumber _ | EBool _ | ENil _ | EId _ -> e
  and helpB bindings =
    match bindings with
    | [] -> []
    | (b, e, l)::rest -> (b, helpE e, l) :: helpB rest
  and helpD (d : sourcespan decl) (* other parameters may be needed here *) =
    match d with
    | DFun(name, args, scheme, body, pos) ->
        let final_body = ELambda(args, helpE body, pos) in
        let this_let_binding = [((BName(name, TyArr((makeBlanks (List.length args) pos), TyBlank(pos), pos), pos)), final_body, pos)] in
        this_let_binding
    | DClass(name, fields, methods, pos) -> []
    | DClassE(name, fields, methods, super, pos) -> []
  and extractClasses decls =
    match decls with
    | DClass(n, b, d, p)::empty -> [DClass(n, b, d, p)]
    | DClassE(n, b, d, e, p)::empty -> [DClassE(n, b, d, e, p)]
    | _ -> []
  in
  match p with
  | Program(tydecls, decls, body, t) ->
          let new_decls = List.flatten(List.map (fun group -> List.map helpD group) decls) in
          let new_body = if List.length new_decls > 0 then ELetRec(List.concat new_decls, (helpE body), t) else helpE body in
          let classes = (List.map extractClasses decls) in
          let new_p = Program(tydecls, classes, new_body, t) in
          debug_printf "BEFORE DESUGAR_PRE: %s\n" (string_of_program p);
          debug_printf "AFTER DESUGAR_PRE: %s\n" (string_of_program new_p);
          new_p
;;

let reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  [
    IMov(Reg(EAX), LabelContents("HEAP_END"));
    ISub(Reg(EAX), Const(size));
    ICmp(Reg(EAX), Reg(ESI));
    IJge(Label ok);
    IPush(Reg(ESP)); (* stack_top in C *)
    IPush(Reg(EBP)); (* first_frame in C *)
    IPush(Const(size)); (* bytes_needed in C *)
    IPush(Reg(ESI)); (* alloc_ptr in C *)
    ICall(Label("try_gc"));
    IAdd(Reg(ESP), Const(16)); (* clean up after call *)
    (* assume gc success if returning here, so EAX holds the new ESI value *)
    IMov(Reg(ESI), Reg(EAX));
    ILabel(ok);
  ]
;;
(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
(* Additionally, you are provided an initial environment of values that you may want to
   assume should take up the first few stack slots.  See the compiliation of Programs
   below for one way to use this ability... *)
let rec compile_method (fun_name : string) body args env is_entry_point fields: instruction list =
  let stack_offset = word_size*((count_vars body)+1) in
  let newEnv = List.flatten (List.mapi (fun i field -> [(field, RegOffset((i) * word_size, ECX))] ) fields) in
  let args_env = List.flatten (List.mapi (fun i arg -> [(arg, RegOffset((i+3) * word_size, EBP))] ) (List.rev args)) in 
  let lbl = 
      if (is_entry_point=true) then
        ILabel(fun_name)
      else
        ILabel(fun_name)
  in
  let stack_setup_asm = [
      lbl;
      ILineComment(Printf.sprintf "Stack_setup for %s" fun_name);
      IPush(Reg(EBP));
      IMov(Reg(EBP), Reg(ESP));
      ISub(Reg(ESP), HexConst(stack_offset));
      IMov(Reg(ECX), RegOffset(8, EBP))

    ]
  and postlude_asm = [
      ILineComment(Printf.sprintf "Clean up for %s" fun_name);
      IAdd(Reg(ESP), HexConst(stack_offset)); 
      IMov(Reg(ESP), Reg(EBP));
      IPop(Reg(EBP));
      IInstrComment(IRet, Printf.sprintf "Return for %s" fun_name);
  ]
  and body_asm = compile_aexpr body 1 (newEnv@env@args_env) (List.length args) true [] StringMap.empty [] in 
    stack_setup_asm
    @ [IInstrComment(ILabel(Printf.sprintf "fun_%s_body" fun_name), Printf.sprintf "Body for %s" fun_name)]
    @ body_asm
    @ postlude_asm;

 and  compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) classobjenv table newclassbindigs: instruction list = 
  match e with 
  | ALet(name, bind, body, _)  -> 
  let prelude = (compile_cexpr bind (si + 1) env num_args false [] classobjenv table newclassbindigs) in
  let body = (compile_aexpr body (si + 1) ((name,RegOffset(~-word_size * (si), EBP))::env) num_args is_tail classobjenv table newclassbindigs)   in
  prelude @ [IMov(RegOffset(~-word_size * (si), EBP), Reg(EAX))] @ body
  | ACExpr(body) -> compile_cexpr body si env num_args is_tail [] classobjenv table newclassbindigs
  | ALetRec(cexplist, body,_) ->
      let newEnv  = env@ (List.flatten (List.mapi (fun i (str,_) -> [(str,RegOffset(~-word_size * (si+i), EBP))]) cexplist)) in 

      let compile_bindings =  List.flatten (List.mapi (fun i (self,cexp) ->
                                  (compile_cexpr cexp (si + i + 1) newEnv num_args is_tail [self] classobjenv table newclassbindigs)@
                                  [ILineComment("move EAX to EBP")]@
                                  [IMov(RegOffset(~-word_size * (si+i), EBP), Reg(EAX))]) cexplist) in 

      let compile_body =  (compile_aexpr body (si + 1 + (List.length cexplist)) newEnv num_args is_tail classobjenv table newclassbindigs) in 

        [ILineComment("compile bindings start ")]@compile_bindings@[ILineComment("compile bindings end ")]@compile_body


  | ASeq(left, right, _) -> (compile_cexpr left si env num_args is_tail [] classobjenv table newclassbindigs)@
                            (compile_aexpr right si env num_args is_tail classobjenv table newclassbindigs)
  | ANewObject(name,obj,classname,body,_) -> [
                                            IMov(Reg(EAX),(find  classobjenv classname));
                                            IMov(Reg(ESI), Reg(EAX));
                                            IAdd(Reg(ESI), RegOffset(word_size, ESI));
                                            IMov(Reg(EAX), Reg(ESI));
                                            IMov(RegOffset(~-word_size * (si), EBP), Reg(EAX));
                                          ]

  @ compile_aexpr body (si+1) ((name,RegOffset(~-word_size * (si), EBP))::env) num_args false classobjenv table ((name,classname)::newclassbindigs)


and compile_cexpr (e : tag cexpr) si env num_args is_tail self classobjenv table newclassbindigs: instruction list = match e with 
  | CIf(cond, _then, _else, tag) ->
      let true_label  =  sprintf "if_true_%s" (string_of_int tag) in
      let false_label = sprintf "if_false_%s" (string_of_int tag) in
      let done_label  =  sprintf "if_done_%s" (string_of_int tag) in
      [IMov(Reg(EAX),(compile_imm cond env classobjenv table));
       ICmp(Reg(EAX), const_true);
       IJne(Label(false_label));
       ILabel(true_label)] @ 
      (compile_aexpr _then (si + 1) env num_args is_tail classobjenv table newclassbindigs) @
      [IJmp(Label(done_label));
       ILabel(false_label);
       ICmp(Reg(EAX), const_false);
       IJne(Label("error_not_boolean_if"))] @
      (compile_aexpr _else (si + 2) env num_args is_tail classobjenv table newclassbindigs) @
      [ILabel(done_label)]

  | CPrim1(op, e, tag) -> 
     begin match op with
      |Add1 -> 
        [IMov(Reg(EAX),(compile_imm e env classobjenv table))] @ 
        [
        ITest(Reg(EAX),tag_as_bool);
        IJnz(Label("arithmetic_expected_a_number"));
        IAdd(Reg(EAX),Const(2));
        IJo(Label("overflow"))
       ] 
      |Sub1 -> 
        [IMov(Reg(EAX),(compile_imm e env classobjenv table))] @ 
        [ITest(Reg(EAX),tag_as_bool);
        IJnz(Label("arithmetic_expected_a_number"));
        ISub(Reg(EAX),Const(2));
        IJo(Label("overflow"))
        ]

      |IsBool -> 
        let not_bool_label = sprintf "isBOOL_false_%s" (string_of_int tag) in
        let done_label = sprintf "isBool_done_%s" (string_of_int tag) in
       [IMov(Reg(EAX),(compile_imm e env classobjenv table))] @  [
        ITest(Reg(EAX), tag_as_bool);
        IJz(Label (not_bool_label));
        IMov(Reg(EAX),const_true);
        IJmp(Label(done_label));
        ILabel(not_bool_label);
        IMov(Reg(EAX),const_false);
        ILabel(done_label)]
      |IsNum -> 
         let  isNum_label = sprintf "isNumtrue_%s" (string_of_int tag) in
         let done_label = sprintf "isNumdone_%s" (string_of_int tag) in
         [IMov(Reg(EAX),(compile_imm e env classobjenv table))] @ [
         ITest(Reg(EAX), tag_as_bool);
         IJz(Label(isNum_label));
         IMov(Reg(EAX),const_false);
         IJmp(Label(done_label));
         ILabel(isNum_label);
         IMov(Reg(EAX),const_true);
         ILabel(done_label)]
      |Not -> 
        [IMov(Reg(EAX),(compile_imm e env classobjenv table))] @ [
        ITest(Reg(EAX), tag_as_bool);
        IJz(Label("logic_expected_a_boolean"));
        IXor(Reg(EAX),bool_mask)
       ]
      |PrintStack -> failwith "print stack"
      |Print -> 
        [
          IMov(Reg(EAX),(compile_imm e env classobjenv table))] @ 
        [
         IPush(Reg(EAX));
         ICall(Label("print"));
         IAdd(Reg(ESP),Const(4))
         ]

    end

  | CPrim2(op, left, right, tag) -> 
    let instr =
        [IMov(Reg(EAX),(compile_imm left  env classobjenv table))] @
        [IMov(RegOffset(~-word_size * (si), EBP), Reg(EAX))] @
        [IMov(Reg(EAX),(compile_imm right env classobjenv table))] @
        [IMov(RegOffset(~-word_size * (si + 1), EBP), Reg(EAX))] in
   begin match op with
   | Plus -> instr @
      [ 
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number_EDX"));
            IAdd(Reg(EAX), Reg(EDX));
            IJo(Label("overflow"))


        ]
   | Minus -> instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number_EDX"));
            ISub(Reg(EAX), Reg(EDX));
            IJo(Label("overflow"))

     ]
   | Times -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("arithmetic_expected_a_number_EDX"));
            IMul(Reg(EAX), Reg(EDX));
            IJo(Label("overflow"));
            ISar(Reg(EAX),Const(1));
            IJo(Label("overflow"))
     ]
   | And -> 
      instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz(Label("logic_expected_a_boolean"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz(Label("logic_expected_a_boolean_edx"));
            IAnd(Reg(EAX), Reg(EDX))
     ]
   | Or -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz(Label("logic_expected_a_boolean"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz(Label("logic_expected_a_boolean_edx"));
            IOr(Reg(EAX), Reg(EDX))
     ]
   | Greater -> 

    let greater_label = sprintf "greater_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number_EDX"));
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJg(Label(greater_label));
            IMov(Reg(EAX), const_false);
            ILabel(greater_label)

     ]
   | GreaterEq -> 

    let greatereq_label = sprintf "greaterequal_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number_EDX"));
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJge(Label(greatereq_label));
            IMov(Reg(EAX), const_false);
            ILabel(greatereq_label)

     ]
   | Less -> 
    let less_label = sprintf "less_%s" (string_of_int tag) in

    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size *(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size *(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number_EDX"));
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJl(Label(less_label));
            IMov(Reg(EAX), const_false);
            ILabel(less_label)

     ]
   | LessEq -> 

    let lesseq_label = sprintf "lessequal_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size *(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number"));
            IMov(Reg(EDX), RegOffset(~-word_size *(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz(Label("comparison_expected_a_number_EDX"));
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJle(Label(lesseq_label));
            IMov(Reg(EAX), const_false);
            ILabel(lesseq_label)

     ]
   | Eq -> 

     instr @
    [
            IMov(Reg(EAX),  RegOffset(~-word_size * (si), EBP));
            IMov(Reg(EDX),  RegOffset(~-word_size * (si + 1), EBP));
            IPush(Reg(EAX));
            IPush(Reg(EDX));
            ICall(Label("equal"));
            IAdd(Reg(ESP), Const(8))
     ]
    |Instanceof -> failwith "implement instance of "

   end
    | CApp(imm_name, imm_args, _) ->
  

        let fname = compile_imm imm_name env classobjenv table in
        let args = List.map  (fun e -> IPush(Sized(DWORD_PTR, (compile_imm e env classobjenv table))))  (List.rev imm_args) in
         
         [
                ILineComment("calling functions");
                IMov(Reg(ECX),fname);
                IAnd(Reg(ECX), HexConst(0x7));
                ICmp(Reg(ECX), HexConst(0x5));
                IJne(Label("error_not_closure"));

                IMov(Reg(EAX), fname);
                ISub(Reg(EAX), HexConst(0x5));
                ICmp(Sized(DWORD_PTR, RegOffset(0, EAX)), Const(List.length imm_args));
                IJne(Label("error_wrong_arity"))
         ] @ args @ 
         [
                IMov(Reg(EAX), fname);
                IPush(Sized(DWORD_PTR,Reg(EAX)));
                ISub(Reg(EAX), HexConst(0x5));
                ICall(RegOffset(4, EAX));
                IAdd(Reg(ESP),Const((List.length imm_args * word_size) + word_size))
         ]

  | CLambda(args,body,tag) ->  
        

          (*helper functions *)
         let moveClosureVarToStack (i: int) : instruction list = 
          [
               IMov(Reg(EDX), RegOffset(16 + (word_size*i), ECX));
               IMov(RegOffset(~-word_size * (i + 1), EBP), Reg(EDX))
          ] in    

          (*labels*)
          let inner_lambda_label = sprintf "inner_lambda_%s" (string_of_int tag) in
          let inner_lambda_end_label = sprintf "inner_lambda_end_%s" (string_of_int tag) in  

          (*create env*)
          let frees = List.sort compare (free_vars_E  body (args@self))  in
          (*List.map (fun e -> debug_printf "%s\n" e) frees;*) 
          let free_env = List.flatten (List.mapi (fun i fv -> [(fv, RegOffset(~-word_size * (i + 1), EBP))] ) frees) in 
          let args_env = List.flatten (List.mapi (fun i arg -> [(arg, RegOffset((i+3) * word_size, EBP))] ) (List.rev args)) in 
       
          let copy_free_to_stack = List.flatten (List.mapi (fun  i  a -> moveClosureVarToStack i) frees) in 

          let (self_env, self_setup) = if List.length self = 1 then 
           ([(  List.hd self,  RegOffset(~-(List.length frees + 1) * word_size , EBP))],

            [
            IMov(Reg(EDX), RegOffset(12, ECX));
            IMov(RegOffset(~-(List.length frees + 1) * word_size , EBP), Reg(EDX));


            ])   
          else ([],[]) in 

          let newEnv =   (args_env@self_env@free_env)  in 

          (* function compilation start*)
          let inner_lambda_stack_setup =  [
               ILabel (inner_lambda_label);
               IPush(Sized(DWORD_PTR, Reg(EBP)));
               IMov(Reg(EBP),Reg(ESP));
               ISub(Reg(ESP),Const((List.length frees + 1) * word_size) );
               IMov(Reg(ECX), RegOffset(8, EBP));
               ISub(Reg(ECX), HexConst(0x5));

           ] @self_setup@copy_free_to_stack in  

          let compileBody = compile_aexpr body (List.length frees + 2) newEnv num_args is_tail classobjenv table newclassbindigs in

          let inner_lambda_epilogue =  [ 
                  IMov(Reg(ESP), Reg(EBP));
                  IPop(Reg(EBP));
                  IRet
            ] in

          let lambda_section = (inner_lambda_stack_setup @ compileBody @ inner_lambda_epilogue) in 
           (* function compilation end*)

           (*create closure*)
          let free_moves =  List.flatten (List.map (fun x -> [find env x]) frees) in
          let move_frees_to_closure = List.flatten (List.mapi (fun i fva -> 
                                        [
                                          IMov(Reg(EAX),fva);
                                          IMov(RegOffset(16 + (word_size*i), ESI), Reg(EAX))
                                        ]
                                    ) free_moves ) in 

          let closure_prologue = 
            [
              ILabel(inner_lambda_end_label);


              IMov(RegOffset(0, ESI), Sized(DWORD_PTR, Const(List.length args)));
              IMov(RegOffset(4, ESI), Sized(DWORD_PTR, Label(inner_lambda_label)));
              IMov(RegOffset(8, ESI), Sized(DWORD_PTR, Const(List.length frees)));
             

            ]
            @ move_frees_to_closure @ 
            [
              IMov(RegOffset(12 ,ESI), Reg(ESI));
              IAdd(RegOffset(12 ,ESI), Sized(DWORD_PTR, HexConst(0x5)));

              IAdd(Reg(EAX), HexConst(0x5));
              
              IAdd(Reg(ESI), Const((List.length frees * word_size) + 16));
              IAdd(Reg(ESI), HexConst(0x7));
              IAnd(Reg(ESI), HexConst(0xFFFFFFF8))
            ]
           (*create closure end*)

          in
          [IJmp(Label(inner_lambda_end_label))] @ lambda_section  @ closure_prologue
   
  | CImmExpr(e) -> [IMov(Reg(EAX),(compile_imm e env classobjenv table))]
  | CMethodCall(obj,funname,imm_args,objectname,_) -> 
    let classname = (find newclassbindigs objectname) in 
    let methodindex  = StringMap.find funname (StringMap.find  classname table)  in 
    let args = List.map  (fun e -> IPush(Sized(DWORD_PTR, (compile_imm e env classobjenv table))))  (List.rev imm_args) in

    [
        IMov(Reg(EAX), (compile_imm obj env classobjenv table))
    ] @ args @
    [
        IPush(Sized(DWORD_PTR,Reg(EAX)));
        ISub(Reg(EAX), HexConst(0x1));
        ICall(RegOffset((methodindex * word_size), EAX));
        IAdd(Reg(ESP),Const((List.length imm_args * word_size) + word_size))

    ]
  | CSetField(obj,fieldname, arg,objectname,_) -> 
    let classname = (find newclassbindigs objectname) in 
    let fieldindex  = StringMap.find fieldname (StringMap.find classname table)  in 
    [
        IMov(Reg(EAX), compile_imm obj env classobjenv table);
        IMov(RegOffset((fieldindex * word_size), EAX), compile_imm arg env classobjenv table)
    ]
 and compile_imm (e : tag immexpr)  (env : arg envt)  classobjenv table = match e with
  | ImmNum(n, _) -> Const((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)
  | ImmNil(_) -> HexConst(0x1)
  | ImmObj(x,_) ->  (find env x)
 ;;

let compile_class dclass = match  dclass with
| AClass(name,fields,methods,tag) -> 
     let fields_size = (List.length fields) * word_size in 
     let fields_names = List.flatten (List.map (fun e -> match e with 
                                    |(str,_)-> [str]) fields )   in 

     let classmethods = List.flatten (List.map (fun f -> match f with
                                    |ADFun(name,args,body,tag) -> 
                                        compile_method name  body args [] false fields_names ) methods) in  
     let create_objectprototype = [IMov(RegOffset(0, ESI), Sized(DWORD_PTR, Const(fields_size + (word_size * (List.length methods)))))]
                                  @ (List.flatten (List.mapi (fun i f ->  match f with
                                    |ADFun(name,args,body,tag) -> 

                                        [
                                          IMov(Reg(EAX),  Label(name));
                                          IMov(RegOffset(4 + fields_size +  (word_size*i), ESI), Reg(EAX))
                                        ]
                                    ) methods ) @
            [
              IMov(Reg(EAX), Reg(ESI)); 
              IAdd(Reg(EAX), HexConst(0x1));
              IAdd(Reg(ESI), Const((List.length methods *  word_size) + fields_size));
              IMov(RegOffset(~-word_size * 1, EBP), Reg(EAX))
            ])

   in 
    let inner_hash =StringMap.empty in 
    let names = (fields_names @ List.flatten( List.map(fun f ->  match f with
                                    |ADFun(name,args,body,tag) ->  [name]) methods )) in 
    let (mapping,_)  = List.fold_left (fun (acc,i) name ->  (StringMap.add name (i+1) acc,i+1)) (inner_hash,0) names  in 
    let outter_init =  StringMap.empty in 
    let table = StringMap.add name mapping outter_init in 
    (create_objectprototype, classmethods, [(name,RegOffset(~-word_size * 1, EBP))], table)
 |AClassE(name,fields,methods,inheritance,tag) -> failwith "class inheritance"
let compile_prog (anfed : tag aprogram) : string = match anfed with
  | AProgram(decls,body,_)  -> 
  let prelude =
 "section .text
  extern error
  extern print
  extern print_stack
  extern equal
  extern try_gc
  extern naive_print_heap
  extern HEAP
  extern HEAP_END
  extern STACK_BOTTOM
  global our_code_starts_here" in
  let count = word_size *  count_vars body in
  let heap_start = [
      IInstrComment(IMov(LabelContents("STACK_BOTTOM"), Reg(EBP)), "This is the bottom of our Garter stack");
      ILineComment("heap start");
      IMov(Reg(ESI),RegOffset(4,ESP));
      IAdd(Reg(ESI),Const(7));
      IAnd(Reg(ESI),HexConst(0xFFFFFFF8))] in 

  let stack_setup = [
      ILineComment("stack start");
      IPush(Reg(EBP));
      IMov(Reg(EBP),Reg(ESP));
      ISub(Reg(ESP),Const(count))] 
    in
  let postlude = [
    ILineComment("postlude start");
    IMov(Reg(ESP),Reg(EBP));
    IPop(Reg(EBP));
    IRet;

    ILabel("logic_expected_a_boolean");
    IPush(Reg(EAX));
    IPush(Const(err_LOGIC_NOT_BOOL));
    ICall(Label("error"));
    ILabel("logic_expected_a_boolean_edx");
    IPush(Reg(EDX));
    IPush(Const(err_LOGIC_NOT_BOOL));
    ICall(Label("error"));

    ILabel("error_not_boolean_if");
    IPush(Reg(EAX));
    IPush(Const(err_IF_NOT_BOOL));
    ICall(Label("error"));

    ILabel("arithmetic_expected_a_number");
    IPush(Reg(EAX));
    IPush(Const(err_ARITH_NOT_NUM));
    ICall(Label("error"));

    ILabel("arithmetic_expected_a_number_EDX");
    IPush(Reg(EDX));
    IPush(Const(err_ARITH_NOT_NUM));
    ICall(Label("error"));

    ILabel("comparison_expected_a_number");
    IPush(Reg(EAX));
    IPush(Const(err_COMP_NOT_NUM));
    ICall(Label("error"));

    ILabel("comparison_expected_a_number_EDX");
    IPush(Reg(EDX));
    IPush(Const(err_COMP_NOT_NUM));
    ICall(Label("error"));

    ILabel("overflow");
    IPush(Reg(EAX));
    IPush(Const(err_OVERFLOW));
    ICall(Label("error"));

    ILabel("error_not_tuple");
    IPush(Reg(ECX));
    IPush(Const(err_GET_NOT_TUPLE));
    ICall(Label("error"));

    ILabel("index_too_high");
    IPush(Reg(EDX));
    IPush(Const(err_GET_HIGH_INDEX));
    ICall(Label("error"));

    ILabel("index_too_low");
    IPush(Reg(EAX));
    IPush(Const(err_GET_LOW_INDEX));
    ICall(Label("error"));

    ILabel("error_not_closure");
    IPush(Reg(ECX));
    IPush(Const(error_not_closure));
    ICall(Label("error"));

    ILabel("error_wrong_arity");
    IPush(Reg(EAX));
    IPush(Const(error_wrong_arity));
    ICall(Label("error"));
    ] in
  let (create_objectprototype, classmethods, classobjenv, table) = compile_class (List.hd decls) in 
  let body = [ILineComment("body start")] @ (compile_aexpr body 2 [] 0 true classobjenv table []) in
  let as_assembly_string = (to_asm  (classmethods @  [ILabel("our_code_starts_here")] @ heap_start  @   stack_setup @ create_objectprototype @ body @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string
  
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  (* |> (add_err_phase well_formed is_well_formed)*)
  |> (add_phase desugared_preTC desugarPre)
  |> (add_phase desugared_postTC desugarPost)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename_and_tag)
  |> (add_phase anfed (fun p -> (atag (anf p))))
  |> (add_phase result compile_prog)
;;
