open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors
(* Add at least one of these two *)
(* open TypeCheck *)
(* open Inference *)
       
type 'a envt = (string * 'a) list

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;


let const_true = HexConst (0xFFFFFFFF)
let const_false = HexConst(0x7FFFFFFF)
let bool_mask = HexConst(0x80000000)
let tag_as_bool = HexConst(0x00000001)

let err_COMP_NOT_NUM   = 0
let err_ARITH_NOT_NUM  = 1
let err_LOGIC_NOT_BOOL = 2
let err_IF_NOT_BOOL    = 3
let err_OVERFLOW       = 4



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
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ACExpr e -> helpC e
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
  and helpB env b =
    match b with
    | BBlank(typ, tag) -> (b, env)
    | BName(name, typ, tag) ->
       let name' = sprintf "%s_%d" name tag in
       (BName(name', typ, tag), (name, name') :: env)
    | BTuple(binds, tag) ->
       let (binds', env') = helpBS env binds in
       (BTuple(binds', tag), env')
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
    | ETuple(es, tag) -> ETuple(List.map (helpE env) es, tag)
    | EGetItem(e, idx, len, tag) -> EGetItem(helpE env e, idx, len, tag)
    | ESetItem(e, idx, len, newval, tag) -> ESetItem(helpE env e, idx, len, helpE env newval, tag)
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
  in (rename [] p)
;;



(* IMPLEMENT EVERYTHING BELOW *)

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
                      | BBlank(_, _) -> raise (InternalCompilerError "Can't have blank in fn call o.O")
                      | BTuple(_, tag) -> raise (InternalCompilerError (sprintf "Desugaring must handle this!! Tag:%d" tag))) args in
       ADFun(name, args, helpA body, ())
  and helpC (e : tag expr) : (unit cexpr * (string * unit cexpr) list) = 
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
    | ELet((b1, exp, _)::rest, body, pos) ->
        let bind = (match b1 with
            | BBlank(t, _) -> (sprintf "_%d" pos)
            | BName(name, _, _) -> name
            | BTuple(_, tag) -> raise (InternalCompilerError (sprintf "Desugaring must handle this!! Tag:%d" tag)))in
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EApp(funname, args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(funname, new_args, ()), List.concat new_setup)
    | ESeq(_, _, tag) -> raise (InternalCompilerError (sprintf "Desugaring must take care of sequences!! Tag:%d" tag))
    | ETuple(expr_list, _) ->
        let (tup_args, tup_setup) = List.split (List.map helpI expr_list) in
        (CTuple(tup_args, ()), List.concat tup_setup)
    | EGetItem(e, idx, len, a) ->
        let (e_imm, e_setup) = helpI e in
        (CGetItem(e_imm, idx, ()), e_setup)
    | ESetItem(e, idx, len, newval, a) ->
        let (e_imm, e_setup) = helpI e in
        let (new_imm, new_setup) = helpI newval in
        (CSetItem(e_imm, idx, new_imm, ()), e_setup @ new_setup)
    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * (string * unit cexpr) list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | EAnnot(e, _, _) -> helpI e
    | ENil(_,_) -> (ImmNil(),[])
    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [(tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [(tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [(tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(funname, args, tag) ->
       let tmp = sprintf "app_%d" tag in
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (ImmId(tmp, ()), (List.concat new_setup) @ [(tmp, CApp(funname, new_args, ()))])
    | ELet([], body, _) -> helpI body
    | ELet((b1, exp, _)::rest, body, pos) ->
        let bind = (match b1 with
            | BBlank(t, _) -> (sprintf "_%d" pos)
            | BName(name, _, _) -> name
            | BTuple(_, tag) -> raise (InternalCompilerError (sprintf "Desugaring must handle this!! Tag:%d" tag))) in
        let (exp_ans, exp_setup) = helpC exp in
        let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | ESeq(_, _, tag) -> raise (InternalCompilerError (sprintf "Desugaring must take care of sequences!! Tag:%d" tag))
    | ETuple(expr_list, tag) ->
        let tmp = sprintf "tup_%d" tag in
        let (tup_args, tup_setup) = List.split (List.map helpI expr_list) in
        (ImmId(tmp, ()), (List.concat tup_setup) @ [(tmp, CTuple(tup_args, ()))])
    | EGetItem(e, idx, len, a) ->
        let tmp = sprintf "eget_%d" a in
        let (e_imm, e_setup) = helpI e in
        (ImmId(tmp, ()), e_setup @ [(tmp, CGetItem(e_imm, idx, ()))])
    | ESetItem(e, idx, len, newval, a) ->
        let tmp = sprintf "eset_%d" a in
        let (e_imm, e_setup) = helpI e in
        let (new_imm, new_setup) = helpI newval in
        (ImmId(tmp, ()), e_setup @ new_setup @ [(tmp, CSetItem(e_imm, idx, new_imm, ()))])
  and helpA e : unit aexpr = 
    let (ans, ans_setup) = helpC e in
    List.fold_right (fun (bind, exp) body -> ALet(bind, exp, body, ())) ans_setup (ACExpr ans)
  in
  helpP p
;;

let rec strip_binds arg =
  let result = (List.fold_left (fun env b -> match b with 
  |BBlank(_,_) -> env
  |BName( str, typ , loc) ->  [(str , loc)]@env
  |BTuple(binds,loc) ->  env @ (strip_binds binds) ) [] arg) in result

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
    | EApp(funname, appargs, pos) -> 
       begin match find_decl ds funname with
       |None -> [UnboundFun(funname, pos)]
       |Some(DFun(name, defargs,_, body, dpos)) -> 
         let no_appargs = List.length appargs in
         let no_defargs = List.length defargs  in
         if no_defargs = no_appargs then [] else
         [Arity (no_appargs,no_defargs,pos)]
      end
    |ELet([], body, pos) -> wf_E body ds env tydecls
    |ELet((BBlank(typ, _), exp, bindloc)::rest as binds, body, pos) -> []
    |ELet((BTuple(blst, _), exp, bindloc)::rest, body, pos) -> []
    |ELet((BName(bind, _, _), exp, bindloc)::rest as binds, body, pos) -> 
      let(exnbinds_list,newenv) = (List.fold_left (fun  (exnlist,env) (b: 'a binding) -> match b with 
      |(BTuple(blst,_),e,y) -> ((wf_E e ds env tydecls), env)
      |(BBlank(_,_),e,y) -> ((wf_E e ds env tydecls), env)
      |(BName(n,typ,loc),e,y) -> match find2 env n with 
      |None -> (exnlist, [(n,y)]@env)
      |Some(exploc) ->  ( (wf_E e ds ([(n,y)] @ env) tydecls) @ [DuplicateId(n,y,exploc)] @exnlist , [(n,y)] @ env)) ([], env) binds) 
    and shadowlist = match find2 env bind with
      |None -> wf_E exp ds env tydecls
      |Some(exploc) -> [ShadowId(bind,bindloc,exploc)]
    in (shadowlist @ exnbinds_list @ (wf_E body ds newenv tydecls)) 
   | ESeq(left, right,_) -> wf_E left ds env  tydecls @ wf_E right ds env tydecls
   | ETuple(exprlist, _) -> List.fold_left (fun lst e -> wf_E e ds env tydecls) [] exprlist
   | EGetItem(tuple,index,size,loc)-> 
     if ( index >=  size) 
     then  wf_E tuple ds env tydecls @ [IndexTooLarge(index,loc)]
     else if (index < 0) then wf_E tuple ds env tydecls @ [IndexTooSmall(index,loc)] else  wf_E tuple ds env tydecls
   | ESetItem (exp,index,size,exp2,loc) -> 
     if (index >=  size) 
     then  wf_E exp ds env tydecls @ [IndexTooLarge(index,loc)] @  wf_E exp2 ds env tydecls
     else if (index < 0) then wf_E exp ds env tydecls @ [IndexTooSmall(index,loc)]  @  wf_E exp2 ds env tydecls
     else wf_E exp ds env tydecls @  wf_E exp2 ds env tydecls
   | ENil(typ,_) ->  wf_T typ  tydecls

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
    | TyCon(str,loc) -> [Unsupported(str,loc)]
    | TyVar(name,loc)->  begin match findecls tydecls name with
                        |None -> [InvalidTyLen(name, loc)]
                        |Some(x) -> [] end
    | TyArr(typlist,typ,_)-> []
    | TyApp(typ,typlist,_) -> []
    | TyTup(typlst,_)-> List.fold_left (fun errlst t -> errlst@  wf_T t tydecls) [] typlst
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
     let output = wf_TD tydecls @  wf_G decls tydecls @ wf_E body (List.flatten decls) [] tydecls in
     if output = [] then Ok(p) else Error(output)
;;

let rec tupArgs arg_lst =
    match arg_lst with
    | first::rest ->
        (match first with
        | BTuple(_, _) -> first :: (tupArgs rest)
        | _ -> tupArgs rest)
    | [] -> []
;;

let rec replaceTups arg_lst ctr =
    let tmp_tup_name = (sprintf "desugar_args_%d" ctr) in
    match arg_lst with
    | first::rest ->
        (match first with
        | BTuple(_, loc) -> BName(tmp_tup_name, bind_to_typ first, loc) :: (replaceTups rest (ctr + 1))
        | BName(_, _, _) -> first :: (replaceTups rest ctr)
        | _ -> raise (InternalCompilerError "BBlank in function definition o.O"))
    | [] -> []
;;

let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    (fun name ->
      next := !next + 1;
      sprintf "%s_%d" name (!next)) in
  let rec helpE (e : sourcespan expr) (* other parameters may be needed here *) =
    match e with
    | ESeq(e1, e2, loc) ->
        let e1_desugar = helpE e1 in
        let seq_blank = (BBlank(TyBlank(loc), loc), e1_desugar, loc) in
        ELet([seq_blank], helpE e2, loc)
    | ELet(bindings, e, loc) ->
        let tmp_name = gensym "desugar_let" in
        let new_bindings = expandBinding bindings tmp_name (List.length bindings) in
        ELet(new_bindings, helpE e, loc)
    | ETuple(expr_lst, loc) -> ETuple(List.map helpE expr_lst, loc)
    | EGetItem(eg, i1, i2, loc) -> EGetItem(helpE eg, i1, i2, loc)
    | ESetItem(eg, i1, i2, es, loc) -> ESetItem(helpE eg, i1, i2, helpE es, loc)
    | EPrim1(p1, e, loc) -> EPrim1(p1, helpE e, loc)
    | EPrim2(p2, e1, e2, loc) -> EPrim2(p2, helpE e1, helpE e2, loc)
    | EIf(c, t, e, loc) -> EIf(helpE c, helpE t, helpE e, loc)
    | EApp(n, el, loc) -> EApp(n, List.map helpE el, loc)
    | EAnnot(e, t, loc) -> EAnnot(helpE e, t, loc)
    | _ -> e
  and expandHelper binds tmp_var ctr len loc=
    match binds with
    | first::rest ->
      let this_expanded_binding =
        (match first with
        | BTuple(_, _) ->
            let tmp_name = gensym "rec_expand" in
            let tmp_binding = [(first, EGetItem(EId(tmp_var, loc), ctr, len, loc), loc)] in
            let this_expanded_list = expandBinding tmp_binding tmp_name (List.length tmp_binding) in
            this_expanded_list @ expandHelper rest tmp_var (ctr + 1) len loc
        | _ -> [(first, EGetItem(EId(tmp_var, loc), ctr, len, loc), loc)]) in
      (this_expanded_binding @ expandHelper rest tmp_var (ctr + 1) len loc)
    | [] -> []
  and expandBinding binding_left tmp_name len =
    let index = len - (List.length binding_left) in
    let tmp_var = sprintf "%s_%d" tmp_name index in
    match binding_left with
    | first::rest ->
      let (bind, e, loc) = first in
      let e = helpE e in
      (match bind with
      | BTuple(bind_list, loc1) ->
        let first_binding = (BName(tmp_var, bind_to_typ bind, loc1), e, loc) in
        let expanded_bindings = expandHelper bind_list tmp_var 0 (List.length bind_list) loc1 in
        ([first_binding] @ expanded_bindings @ (expandBinding rest tmp_name len))
      | _ -> ([(bind, e, loc)] @ expandBinding rest tmp_name len))
    | _ -> []
  and tupToName binds ctr =
    let tmp_name = (sprintf "desugar_args_%d" ctr) in
    match binds with
    | first::rest ->
        (match first with
        | BTuple(tup_binds, loc) -> BName(tmp_name, TyTup(List.map bind_to_typ tup_binds, loc), loc) :: (tupToName rest (ctr + 1))
        | _ -> tupToName rest ctr)
    | [] -> []
  and helpNewBinds ele1 ele2 =
    match ele1 with
    | BTuple(tup_binds, _) ->
      (match ele2 with
      | BName(tmp, typ, t) ->
        (BTuple(tup_binds, t), EId(tmp, t), t)
      | _ -> raise (InternalCompilerError "Processing args broken o.O"))
    | _ -> raise (InternalCompilerError "tupToName or tupArgs broken O.o")
  and helpArgs args =
    let new_args = (replaceTups args 0) in
    let new_bindings = List.map2 helpNewBinds (tupArgs args) (tupToName args 0) in
    (new_args, new_bindings)
  and helpD (d : sourcespan decl) (* other parameters may be needed here *) =
    match d with
    | DFun(name, args, scheme, body, pos) ->
        let (new_args, new_binds) = helpArgs args in
        let new_body = ELet(new_binds, (helpE body), pos) in
        DFun(name, new_args, scheme, helpE new_body, pos)
  in
  match p with
  | Program(tydecls, decls, body, t) ->
          let new_decls = List.map (fun group -> List.map helpD group) decls in
          let new_body = helpE body in
          let new_p = Program(tydecls, new_decls, new_body, t) in
          new_p
;;

let rec compile_fun (fun_name : string) body args env is_entry_point : instruction list =
  let stack_offset =  ((count_vars body)+1) in
  let lbl =
      if (is_entry_point=true) then
        ILabel(fun_name)
      else
        ILabel("fun_" ^ fun_name)
  in
  let stack_setup_asm = [
      lbl;
      ILineComment(Printf.sprintf "Stack_setup for %s" fun_name);
      IPush(Reg(EBP));
      IMov(Reg(EBP), Reg(ESP));
      ISub(Reg(ESP), HexConst(stack_offset))
    ]
  and postlude_asm = [
      ILineComment(Printf.sprintf "Clean up for %s" fun_name);
      IAdd(Reg(ESP), HexConst(stack_offset)); 
      IMov(Reg(ESP), Reg(EBP));
      IPop(Reg(EBP));
      IInstrComment(IRet, Printf.sprintf "Return for %s" fun_name);
  ]
  and body_asm = compile_aexpr body 1 env (List.length args) true in 
    stack_setup_asm
    @ [IInstrComment(ILabel(Printf.sprintf "fun_%s_body" fun_name), Printf.sprintf "Body for %s" fun_name)]
    @ body_asm
    @ postlude_asm;

and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) : instruction list = match e with
  | ALet(name, bind, body, _)  -> 
  let prelude = (compile_cexpr bind (si + 1) env num_args false) in
  let body = (compile_aexpr body (si + 1) ((name,RegOffset(~-word_size * (si), EBP))::env) num_args is_tail) in
  prelude @ [IMov(RegOffset(~-word_size * (si), EBP), Reg(EAX))] @ body
  | ACExpr(body) -> compile_cexpr body si env num_args is_tail
and compile_cexpr (e : tag cexpr) si env num_args is_tail : instruction list = match e with 
  | CIf(cond, _then, _else, tag) ->
      let true_label  =  sprintf "if_true_%s" (string_of_int tag) in
      let false_label = sprintf "if_false_%s" (string_of_int tag) in
      let done_label  =  sprintf "if_done_%s" (string_of_int tag) in
      [IMov(Reg(EAX),(compile_imm cond env))] @
      [ICmp(Reg(EAX), const_true)] @
      [IJne(false_label)] @
      [ILabel(true_label)] @
      (compile_aexpr _then (si + 1) env num_args is_tail) @
      [IJmp(done_label)] @
      [ILabel(false_label)] @
      [ICmp(Reg(EAX), const_false)] @
      [IJne("error_not_boolean_if")] @
      (compile_aexpr _else (si + 2) env num_args is_tail) @
      [ILabel(done_label)]

  | CPrim1(op, e, tag) -> 
     begin match op with
      |Add1 -> 
        [IMov(Reg(EAX),(compile_imm e env))] @ 
        [
        ITest(Reg(EAX),tag_as_bool);
        IJnz("arithmetic_expected_a_number");
        IAdd(Reg(EAX),Const(2));
        IJo("overflow")
       ] 
      |Sub1 -> 
        [IMov(Reg(EAX),(compile_imm e env))] @ 
        [ITest(Reg(EAX),tag_as_bool);
        IJnz("arithmetic_expected_a_number");
        ISub(Reg(EAX),Const(2));
        IJo("overflow")
        ]

      |Print -> 
        [IMov(Reg(EAX),(compile_imm e env))] @ 
        [IPush(Reg(EAX));
        ICall("print");
        IAdd(Reg(ESP),Const(4))]
      |IsBool -> 
        let not_bool_label = sprintf "isBOOL_false_%s" (string_of_int tag) in
        let done_label = sprintf "isBool_done_%s" (string_of_int tag) in
       [IMov(Reg(EAX),(compile_imm e env))] @  [
        ITest(Reg(EAX), tag_as_bool);
        IJz(not_bool_label);
        IMov(Reg(EAX),const_true);
        IJmp(done_label);
        ILabel(not_bool_label);
        IMov(Reg(EAX),const_false);
        ILabel(done_label)]
      |IsNum -> 
         let  isNum_label = sprintf "isNumtrue_%s" (string_of_int tag) in
         let done_label = sprintf "isNumdone_%s" (string_of_int tag) in
         [IMov(Reg(EAX),(compile_imm e env))] @ [
         ITest(Reg(EAX), tag_as_bool);
         IJz(isNum_label);
         IMov(Reg(EAX),const_false);
         IJmp(done_label);
         ILabel(isNum_label);
         IMov(Reg(EAX),const_true);
         ILabel(done_label)]
      |Not -> 
        [IMov(Reg(EAX),(compile_imm e env))] @ [
        ITest(Reg(EAX), tag_as_bool);
        IJz("logic_expected_a_boolean");
        IXor(Reg(EAX),bool_mask)
       ]
      |IsTuple -> 
        let  istuple_label = sprintf "istuple_label_%s" (string_of_int tag) in
         let done_label = sprintf "istuple_labeldone_%s" (string_of_int tag) in
         [
          IMov(Reg(EAX),(compile_imm e env))] @ 
          [
            IAnd(Reg(EAX), Const(0x00000007));
            ICmp(Reg(EAX), Const(0x00000001));
            IMov(Reg(EAX),const_false);
            IJne(done_label);
            ILabel(istuple_label);
            IMov(Reg(EAX),const_true);
            ILabel(done_label)
          ]


      |PrintStack -> failwith "print stack"
    end

  | CPrim2(op, left, right, tag) -> 
    let instr =
        [IMov(Reg(EAX),(compile_imm left  env))] @
        [IMov(RegOffset(~-word_size * (si), EBP), Reg(EAX))] @
        [IMov(Reg(EAX),(compile_imm right env))] @
        [IMov(RegOffset(~-word_size * (si + 1), EBP), Reg(EAX))] in
   begin match op with
   | Plus -> instr @
      [ 
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("arithmetic_expected_a_number_EDX");
            IAdd(Reg(EAX), Reg(EDX));
            IJo("overflow")


        ]
   | Minus -> instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("arithmetic_expected_a_number_EDX");
            ISub(Reg(EAX), Reg(EDX));
            IJo("overflow")

     ]
   | Times -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("arithmetic_expected_a_number_EDX");
            IMul(Reg(EAX), Reg(EDX));
            IJo("overflow");
            ISar(Reg(EAX),Const(1));
            IJo("overflow")
     ]
   | And -> 
      instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz("logic_expected_a_boolean");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz("logic_expected_a_boolean_edx");
            IAnd(Reg(EAX), Reg(EDX))
     ]
   | Or -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz("logic_expected_a_boolean");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz("logic_expected_a_boolean_edx");
            IOr(Reg(EAX), Reg(EDX))
     ]
   | Greater -> 

    let greater_label = sprintf "greater_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("comparison_expected_a_number_EDX");
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJg(greater_label);
            IMov(Reg(EAX), const_false);
            ILabel(greater_label)

     ]
   | GreaterEq -> 

    let greatereq_label = sprintf "greaterequal_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size * (si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size * (si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("comparison_expected_a_number_EDX");
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJge(greatereq_label);
            IMov(Reg(EAX), const_false);
            ILabel(greatereq_label)

     ]
   | Less -> 
    let less_label = sprintf "less_%s" (string_of_int tag) in

    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size *(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size *(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("comparison_expected_a_number_EDX");
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJl(less_label);
            IMov(Reg(EAX), const_false);
            ILabel(less_label)

     ]
   | LessEq -> 

    let lesseq_label = sprintf "lessequal_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-word_size *(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-word_size *(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("comparison_expected_a_number_EDX");
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJle(lesseq_label);
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
            ICall("equal");
            IAdd(Reg(ESP), Const(8))
     ]
   end
  | CApp(name, exprs, _) ->
  
       if is_tail=true && num_args=(List.length exprs) then
         (
    
          (replace_args exprs env) @ [ IJmp(Printf.sprintf "fun_%s_body" name)]

        )
       else (
         [ILineComment(Printf.sprintf "Prepare to call function fun_%s" name); ] @
         List.flatten (List.map(fun x ->
           [ IMov(Reg(EAX), compile_imm x env ); IInstrComment(IPush(Reg(EAX)), (Printf.sprintf "Argument %s" (string_of_immexpr x))) ] (* Copy off memory into EAX, then push EAX *)
           ) (List.rev exprs) )
         @ [ ICall(Printf.sprintf "fun_%s" name)] @ [ IAdd(Reg(ESP), HexConst(4*(List.length exprs))) ] 
         )

  | CTuple(lst,_)-> 
      let size = [IMov(RegOffset(0, ESI), Sized(DWORD_PTR, Const(List.length lst)))] in
      let args = List.map (fun e ->  compile_imm e env) lst in
      let instr = List.flatten (List.mapi (fun i a -> [
        IMov(Reg(EAX), Sized(DWORD_PTR, a));
        IMov(Sized(DWORD_PTR, RegOffset( (i + 1) * 4, ESI)), Reg(EAX))]) args) in

      let to_tuple = [IMov(Reg(EAX), Reg(ESI)); 
                       IAdd(Reg(EAX), Const(1))] in
      let offset = [IAdd(Reg(ESI), Const(4 * (List.length lst + 1)));
                    IAdd(Reg(ESI), Const(4));
                  ] in

       size @  instr @ to_tuple @ offset
     

  | CGetItem(pair,index,_)-> 
   [

    IMov(Reg(EDX),Const(index));
    IMov(Reg(EAX),HexConst(0x0));
    ICmp(Reg(EDX),Reg(EAX));
    IJl("index_too_low");
    IMov(Reg(EAX),  compile_imm pair env);
    ICmp(Reg(EAX),Reg(EDX));
    IJge("index_too_high");
    ISub(Reg(EAX),HexConst(0x00000001));
    IMov(Reg(EAX),RegOffset(word_size * (index + 1), EAX))
   ]
  | CSetItem(pair,index,newpair,loc)-> 
    [
     IMov(Reg(EAX), compile_imm pair env);
     IMov(Reg(EDX), compile_imm newpair env);
     IMov(Reg(ECX), Reg(EAX));
     IAnd(Reg(ECX), HexConst(0x7));
     ICmp(Reg(ECX),HexConst(0x1));
     IJne("error_not_tuple");
     ISub(Reg(EAX),HexConst(0x1));
     IMov(RegOffset( word_size * (index + 1), EAX),Reg(EDX));
     IAdd(Reg(EAX),HexConst(0x1));
    ]
  | CImmExpr(e) -> [IMov(Reg(EAX),(compile_imm e env))]
 and compile_imm (e : tag immexpr)  (env : arg envt) = match e with
  | ImmNum(n, _) -> Const((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)
  | ImmNil(_) -> HexConst(0x1)
  
and replace_args exprs env = 
  let rec _push_args exprlist = 
    match exprlist with
    | head::tail ->
       [ IMov(Reg(EAX), compile_imm head env);
         IInstrComment(IPush(Reg(EAX)), (Printf.sprintf "Save %s onto our stack" (string_of_immexpr head))) ]
       @ _push_args tail
    | _ -> []
  and
  _replace_with_saved_args exprlist idx = 
    match exprlist with
    | head::tail ->
      [ IPop(Reg(EAX)); 
       IInstrComment(IMov(RegOffset(word_size*(idx+1), EBP), Reg(EAX)), (Printf.sprintf "Argument %s (idx %d)" (string_of_immexpr head) idx)) ]
       @ _replace_with_saved_args tail (idx+1)
    | _ -> []
  in
  _push_args (List.rev exprs)

  @ _replace_with_saved_args exprs 1

let build_env (vars: string list) : (string * arg) list =
  let rec _build_env (vars: string list) (parsed: (string * arg) list) : (string list * (string * arg) list) = 
    match vars with
    | vname::tail -> 
        let offset = 8+4*(List.length parsed) in
        let this_res = [(vname, RegOffset(offset, EBP))] in 
        (_build_env tail (parsed @ this_res)
        )
    | _ -> ([], parsed)
  in let (_, result) = _build_env vars [] in
    result

let rec compile_decl (d: tag adecl ) : instruction list = match d with
  | ADFun(fun_name, args, body, tag) -> 
      let local_env = build_env args in (compile_fun fun_name body args local_env false) 
  
let compile_prog (anfed : tag aprogram) : string = match anfed with
  | AProgram(decls,body,_)  -> 
  let prelude =
 " 
  section .text
  extern equal 
  extern error
  extern print
  global our_code_starts_here" in
  let count = word_size *  count_vars body in
  let heap_start = [
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
    IPush(Const(1));
    ICall("error");
    ILabel("logic_expected_a_boolean_edx");
    IPush(Reg(EDX));
    IPush(Const(1));
    ICall("error");

    ILabel("error_not_boolean_if");
    IPush(Reg(EAX));
    IPush(Const(2));
    ICall("error");

    ILabel("arithmetic_expected_a_number");
    IPush(Reg(EAX));
    IPush(Const(3));
    ICall("error");

    ILabel("arithmetic_expected_a_number_EDX");
    IPush(Reg(EDX));
    IPush(Const(3));
    ICall("error");

    ILabel("comparison_expected_a_number");
    IPush(Reg(EAX));
    IPush(Const(4));
    ICall("error");

    ILabel("comparison_expected_a_number_EDX");
    IPush(Reg(EDX));
    IPush(Const(4));
    ICall("error");

    ILabel("overflow");
    IPush(Reg(EAX));
    IPush(Const(5));
    ICall("error");

    ILabel("error_not_tuple");
    IPush(Reg(EDX));
    IPush(Const(6));
    ICall("error");

    ILabel("index_too_high");
    IPush(Reg(EDX));
    IPush(Const(7));
    ICall("error"); 

    ILabel("index_too_low");
    IPush(Reg(EAX));
    IPush(Const(8));
    ICall("error") 
    ] in
  let fun_def =  List.flatten (List.map compile_decl decls) in
  let body = [ILineComment("body start")] @ (compile_aexpr body 1 [] 0 true) in
  let as_assembly_string = (to_asm (fun_def @ [ILabel("our_code_starts_here")] @ heap_start  @   stack_setup @ body @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string


(* Add a desugaring phase somewhere in here, as well as your typechecker *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (add_err_phase well_formed is_well_formed)
  |> (add_phase desugared desugar)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename_and_tag)
  |> (add_phase anfed (fun p -> atag (anf p)))
  |> (add_phase result compile_prog)
;;
