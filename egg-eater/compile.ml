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

(* Helper function, flatten a list of lists *)
let flatten (l: 'a list list) : 'a list =
  let rec flatten_helper (l: 'a list list) (output: 'a list) : 'a list =
    match l with
    | head::tail -> flatten_helper tail (head@output)
    | [] -> output
  in
  flatten_helper l []

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
    | ELet(_::_, body, _) -> raise (NotYetImplemented "Finish this")
    | EApp(funname, args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(funname, new_args, ()), List.concat new_setup)
    | ESeq(_, _, tag) -> raise (InternalCompilerError (sprintf "Desugaring must take care of sequences!! Tag:%d" tag))
    | ETuple(expr_list, _) ->
        let (tup_args, tup_setup) = List.split (List.map helpI expr_list) in
        (CTuple(tup_args, ()), List.concat tup_setup)
    | EGetItem(e, idx, len, a) -> raise (NotYetImplemented "err")
    | ESetItem(e, idx, len, newval, a) -> raise (NotYetImplemented "err")
    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)

  and helpI (e : tag expr) : (unit immexpr * (string * unit cexpr) list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | EAnnot(e, _, _) -> helpI e

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
    | ELet(_::_, body, _) -> raise (NotYetImplemented "Finish this")
    | ESeq(_, _, tag) -> raise (InternalCompilerError (sprintf "Desugaring must take care of sequences!! Tag:%d" tag))
    | ETuple(expr_list, tag) ->
        let tmp = sprintf "tup_%d" tag in
        let (tup_args, tup_setup) = List.split (List.map helpI expr_list) in
        (ImmId(tmp, ()), (List.concat tup_setup) @ [(tmp, CTuple(tup_args, ()))])
    | EGetItem(e, idx, len, a) -> raise (NotYetImplemented "err")
    | ESetItem(e, idx, len, newval, a) -> raise (NotYetImplemented "err")
    | _ -> raise (NotYetImplemented "Finish the remaining cases")
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


let is_well_formed (p : sourcespan program) : (sourcespan program) fallible =
  let rec wf_E (e: sourcespan expr) (ds : 'a decl list) (env : (string * sourcespan) list) : exn list = match e with
    | ENumber(n, pos) -> if n > 1073741823 || n < -1073741824 then
       [Overflow(n,pos)] else []
    | EBool(_, _) -> []
    | EId(name, pos) -> 
      begin match find2 env name with
      |None -> [UnboundId(name,pos)]
      |Some(d) -> [] end
    | EAnnot(exp, _, _) -> wf_E exp ds env
    | EPrim1(op, arg, _) ->  wf_E arg ds env
    | EPrim2(op, left, right, _) ->  wf_E left ds env @ wf_E right ds env
    | EIf(cond, _then, _else, _) -> wf_E cond ds env @ wf_E _then ds env @ wf_E _else ds env
    | EApp(funname, appargs, pos) -> 
       begin match find_decl ds funname with
       |None -> [UnboundFun(funname, pos)]
       |Some(DFun(name, defargs,_, body, dpos)) -> 
         let no_appargs = List.length appargs in
         let no_defargs = List.length defargs  in
         if no_defargs = no_appargs then [] else
         [Arity (no_appargs,no_defargs,pos)]
      end
    | ELet([], body, _) -> wf_E body ds env
    | ELet(bindlist,body,_) ->     failwith "implement elet"
      (*ELet(((bind, _, _), exp, bindloc)::rest as binds, body, pos) -> 

      let(exnbinds_list,newenv) = (List.fold_left (fun  (exnlist,env) (b: 'a bind) -> match b with 
      |((n,_,_),e,y) -> match find2 env n with 
      |None -> (exnlist, [(n,y)]@env)
      |Some(exploc) ->  ( (wf_E e ds ([(n,y)] @ env)) @ [DuplicateId(n,y,exploc)] @exnlist , [(n,y)] @ env)) ([], env) binds) 
    and shadowlist = match find2 env bind with
      |None -> wf_E exp ds env
      |Some(exploc) -> [ShadowId(bind,bindloc,exploc)]
    in (shadowlist @ exnbinds_list @ (wf_E body ds newenv)))*)
   | ESeq(left, right,_) -> wf_E left ds env @ wf_E right ds env
   | ETuple(exprlist, _) -> List.fold_left (fun lst e -> wf_E e ds env) [] exprlist
   | EGetItem(tuple,index,size,_)-> []
   | ESetItem (exp,val1,val2,exp2,_) -> []
   | ENil(typ,_) ->  []

  and wf_D  (ds : 'a decl list): exn list = 
    let result = 
    (List.fold_left (fun errorlst (DFun(funname,  ars,_, body, upos)) ->
      let args = strip_binds ars in 
      let dupfunlist = match (find_decl (remove_one_decl ds funname) funname)  with
       |None -> errorlst@(wf_E body ds args)
       |Some(DFun(name, _,_, _body, dpos)) -> errorlst@[DuplicateFun(funname,upos,dpos)]@(wf_E body ds args) 
      and dupargslist = (List.fold_left (fun exnlist (arg,argloc) -> match (find2 (remove_one_arg args arg) arg) with
            |None -> errorlst
            |Some(loc) -> errorlst@[DuplicateId(arg,loc,argloc)]
          ) [] args) in (dupfunlist @ dupargslist)) [] ds) in result 
  and wf_G (gds : 'a decl list list ): exn list  =
   let (has_seen, exnlist) = (List.fold_left (fun (has_seen, exnlist) gd -> (gd@has_seen, exnlist@(wf_D (gd@has_seen))))  ([],[]) gds) in exnlist
  
  and wf_T (t : sourcespan typ) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement well-formedness checking for types"])
  and wf_S (s : sourcespan scheme) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement well-formedness checking for typeschemes"])
  and wf_TD (t : sourcespan tydecl) (* other parameters may be needed here *) =
      Error([NotYetImplemented "Implement well-formedness checking for typeschemes"])

  in
  match p with
  | Program(tydecls, decls, body, _) ->
     let output = wf_G decls @ wf_E body (List.flatten decls) [] in
     if output = [] then Ok(p) else Error(output)
;;


let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    (fun name ->
      next := !next + 1;
      sprintf "%s_%d" name (!next)) in
  let rec helpE (e : sourcespan expr) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for expressions"])
  and helpD (d : sourcespan decl) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for definitions"])
  and helpG (g : sourcespan decl list) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for definition groups"])
  and helpT (t : sourcespan typ) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for types"])
  and helpS (s : sourcespan scheme) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for typeschemes"])
  and helpTD (t : sourcespan tydecl) (* other parameters may be needed here *) =
    Error([NotYetImplemented "Implement desugaring for type declarations"])
  in
  match p with
  | Program(tydecls, decls, body, _) ->
      raise (NotYetImplemented "Implement desugaring for programs")
;;


let rec compile_fun (fun_name : string) args env : instruction list =
  let count = (word_size *  List.length args) in
  let stack_setup = [
      IPush(Reg(EBP));
      IMov(Reg(EBP),Reg(ESP));
      ISub(Reg(ESP),Const(count))] in
    [ILabel(fun_name)] @ stack_setup

and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) : instruction list = match e with
  | ALet(name, bind, body, _)  -> 
  let prelude = (compile_cexpr bind (si + 1) env num_args false) in
  let body = (compile_aexpr body (si + 1) ((name,RegOffset(~-si, EBP))::env) num_args is_tail) in
  prelude @ [IMov(RegOffset(~-si, EBP), Reg(EAX))] @ body
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
      |PrintStack -> failwith "print stack"
    end

  | CPrim2(op, left, right, tag) -> 
    let instr =
        [IMov(Reg(EAX),(compile_imm left  env))] @
        [IMov(RegOffset(~-si, EBP), Reg(EAX))] @
        [IMov(Reg(EAX),(compile_imm right env))] @
        [IMov(RegOffset(~-(si + 1), EBP), Reg(EAX))] in
   begin match op with
   | Plus -> instr @
      [ 
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("arithmetic_expected_a_number_EDX");
            IAdd(Reg(EAX), Reg(EDX));
            IJo("overflow")


        ]
   | Minus -> instr @
    [
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("arithmetic_expected_a_number_EDX");
            ISub(Reg(EAX), Reg(EDX));
            IJo("overflow")

     ]
   | Times -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("arithmetic_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
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
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz("logic_expected_a_boolean");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz("logic_expected_a_boolean_edx");
            IAnd(Reg(EAX), Reg(EDX))
     ]
   | Or -> 
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJz("logic_expected_a_boolean");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJz("logic_expected_a_boolean_edx");
            IOr(Reg(EAX), Reg(EDX))
     ]
   | Greater -> 

    let greater_label = sprintf "greater_%s" (string_of_int tag) in
    instr @
    [
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
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
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
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
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
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
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            ITest(Reg(EAX), tag_as_bool);
            IJnz("comparison_expected_a_number");
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            ITest(Reg(EDX), tag_as_bool);
            IJnz("comparison_expected_a_number_EDX");
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJle(lesseq_label);
            IMov(Reg(EAX), const_false);
            ILabel(lesseq_label)

     ]
   | Eq -> 
     let eq_label = sprintf "equal_%s" (string_of_int tag) in
     instr @
    [
            IMov(Reg(EAX), RegOffset(~-(si), EBP));
            (* ITest(Reg(EAX), const_bool_tag); *)
            (* IJnz("comparison_expected_a_number"); *)
            IMov(Reg(EDX), RegOffset(~-(si + 1), EBP));
            (* ITest(Reg(EDX), const_bool_tag); *)
            (* IJnz("comparison_expected_a_number_EDX"); *)
            ICmp(Reg(EAX), Reg(EDX));
            IMov(Reg(EAX), const_true);
            IJe(eq_label);
            IMov(Reg(EAX), const_false);
            ILabel(eq_label)

     ]
   end
  | CApp(funname, args, _) -> 
    let arglist = (List.fold_left (fun lst arg -> lst @ [IMov(Reg(EAX),(compile_imm arg env));IPush(Reg(EAX))]
     )  [] args) in
    arglist @ [ICall(funname);IAdd(Reg(ESP),Const(word_size * List.length args))]
  | CTuple(lst,_)-> 
    [
     IMov(Reg(ESI), (compile_imm  (List.nth lst 0) env));
     IMov(RegOffset(1,ESI), (compile_imm (List.nth lst 1) env));
     IMov(Reg(EAX),Reg(ESI));
     IAdd(Reg(EAX), HexConst(0x1));
     IAdd(Reg(ESI),Const(8))
    ]
  | CGetItem(pair,index,_)-> 
   [
    IMov(Reg(EAX), compile_imm pair env);
    (*TODO add tag check*)
    ISub(Reg(EAX),HexConst(0x1));
    IMov(Reg(EAX),RegOffset(index, EAX))
   ]
  | CSetItem(pair,index,newpair,loc)-> 
    [
     IMov(Reg(EAX), compile_imm pair env);
     IMov(Reg(EDX), compile_imm newpair env);
     IMov(Reg(ECX), Reg(EAX));
     IAnd(Reg(ECX), HexConst(0x7));
     (* TODO add tag checks*)
     ISub(Reg(EAX),HexConst(0x1));
     IMov(RegOffset(index, EAX),Reg(EDX));
     IAdd(Reg(EAX),HexConst(0x1));
    ]
  | CImmExpr(e) -> [IMov(Reg(EAX),(compile_imm e env))]
and compile_imm (e : tag immexpr)  (env : arg envt) = match e with
  | ImmNum(n, _) -> Const((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)
  | ImmNil(_) -> failwith "ImmNil"


let compile_decl (d : tag adecl) : instruction list = match d with
  | ADFun(fname,args,body,_) -> 
    let postlude = [IMov(Reg(ESP),Reg(EBP));IPop(Reg(EBP));IRet] in
   (compile_fun fname args []) @(compile_aexpr body 1 [] 0 true) @ postlude
  
let compile_prog (anfed : tag aprogram) : string = match anfed with
  | AProgram(decls,body,_)  -> 
  let prelude =
        "section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:" in
  let count = word_size *  count_vars body in
  let stack_setup =[
      IPush(Reg(EBP));
      IMov(Reg(EBP),Reg(ESP));
      ISub(Reg(ESP),Const(count));

      IMov(Reg(ESI),RegOffset(8,EBP));
      IAdd(Reg(ESI),Const(7));
      IAnd(Reg(ESI),HexConst(0xfffffff8));

      ] in
  let postlude = [
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
    ICall("error") 
    ] in
  let fun_def = List.flatten (List.map compile_decl decls) in
  let body = (compile_aexpr body 1 [] 0 true) in
  let as_assembly_string = (to_asm (fun_def @ stack_setup @ body @ postlude)) in
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
