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
                      | _ -> raise (NotYetImplemented("Finish this"))) args in
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
    (* | ELet(((bind, _, _), exp, _)::rest, body, pos) ->
     *    let (exp_ans, exp_setup) = helpC exp in
     *    let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
     *    (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup) *)
    | EApp(funname, args, _) ->
       let (new_args, new_setup) = List.split (List.map helpI args) in
       (CApp(funname, new_args, ()), List.concat new_setup)
    (* NOTE: You may need more cases here, for sequences and tuples *)
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
    (* | ELet(((bind, _, _), exp, _)::rest, body, pos) ->
     *    let (exp_ans, exp_setup) = helpC exp in
     *    let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
     *    (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup) *)
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
    Error([NotYetImplemented "Implement well-formedness checking for type declarations"])
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

(* After a mathematical operation, if we overflowed, jump to overflow handler *)
let overflowcheck: instruction list =
    [IJo "err_overflow"]

(* Test to ensure value in EAX contains a number, otherwise jump to our 'interror' error handler *)
(* Then check for overflow *)
let intcheck : instruction list =
 (List.map (fun x -> IInstrComment(x, "\tTypecheck: int")) [ITest(Reg(EAX), HexConst(0x1)); IJnz "err_arith_numb"] @ overflowcheck)

(* Runtime: Check two values are ints, else jump to label. Note these values don't need to be in EAX at the start  *)
let twointcheck (e1) (e2) (label: string) : instruction list =
    (List.map (fun x-> IInstrComment(x, "\tTypecheck: 2 ints")) [IMov(Reg(EAX), e1); ITest(Reg(EAX), HexConst(0x1)); IJnz ("err_"^label^"_numb");
     IMov(Reg(EAX), e2); ITest(Reg(EAX), HexConst(0x1)); IJnz ("err_"^label^"_numb");])

(* Test to ensure value in EAX is a bool, otherwise jump to our 'err_logic_bool' error handler *)
let boolcheck (label: string): instruction list =
  (List.map (fun x -> IInstrComment(x, "\tTypecheck: bool")) [ITest(Reg(EAX), HexConst(0x1)); IJz ("err_" ^ label ^"_bool")])

(* Runtime: Check two values are bools, else jump to label. Note these values don't need to be in EAX at the start *)
let twoboolcheck (e1) (e2) (label: string) : instruction list =
    (List.map (fun x-> IInstrComment(x, "\tTypecheck: 2 bools")) [IMov(Reg(EAX), e1); ITest(Reg(EAX), HexConst(0x1)); IJz ("err_" ^ label^"_bool");
     IMov(Reg(EAX), e2); ITest(Reg(EAX), HexConst(0x1)); IJz ("err_" ^ label^"_bool");])

(* For a prim2, generate the approperiate asm checks *)
let typechecks (op1) (op2) (op: prim2) : instruction list * instruction list =
    match op with
    | And | Or | EqB -> ((twoboolcheck op1 op2 "logic"), [])
    | Greater | GreaterEq | Less | LessEq ->  ((twointcheck op1 op2 "comp"), overflowcheck)
    | Plus | Minus | Times  ->  ((twointcheck op1 op2 "arith"), overflowcheck)
    | Eq -> ([], [])

(* Stringify a prim and tag pair into a label name*)
let cmp_label (e: prim2) (tag: int) : string =
    let op =
      match e with
      | Plus       -> "plus"
      | Minus      -> "minus"
      | Times      -> "times"
      | And        -> "and"
      | Or         -> "or"
      | Greater    -> "greater"
      | GreaterEq  -> "greatereq"
      | Less       -> "less"
      | LessEq     -> "lesseq"
      | Eq         -> "eq"
      | EqB        -> "eqb"
    in
      sprintf "$%s_%d_end" op tag


let compare_vals (l) (r) (cmp: instruction) (end_label: string) : instruction list =
    [ IMov(Reg(EAX), l);                   (* Put left into eax *)
      ICmp(Reg(EAX), r);                  (* compare left to right *)
      IMov(Reg(EAX), const_true);  (* Store true into EAX (doesn't change flags) *)
      cmp;
      IMov(Reg(EAX), const_false);  (* else: store false in EAX *)
      ILabel(end_label);                  (* End label *)
    ]

let rec compile_fun (fun_name : string) body args env is_entry_point : instruction list =
  let stack_offset = 4*((count_vars body)+1) in
  let lbl =
      if (is_entry_point=true) then
        ILabel(fun_name)
      else
        ILabel("fun_" ^ fun_name)
  in
  let stack_setup_asm = [
      (* Main prologue: label, push the old base pointer, copy ESP into EBP, modify ESP to make room for our vars *)
      lbl;
      ILineComment(Printf.sprintf "Stack_setup for %s" fun_name);
      IPush(Reg(EBP));
      IMov(Reg(EBP), Reg(ESP));
      ISub(Reg(ESP), HexConst(stack_offset))
    ]
  (* After function body, clean up the stack and return *)
  and postlude_asm = [
      (* Main epilogue: clean up stack, restore stack pointer and return to parent caller *)
      ILineComment(Printf.sprintf "Clean up for %s" fun_name);
      IAdd(Reg(ESP), HexConst(stack_offset)); (* This isn't right? Pushes for fn args changed things? *)
      IMov(Reg(ESP), Reg(EBP));
      IPop(Reg(EBP));
      IInstrComment(IRet, Printf.sprintf "Return for %s" fun_name);
  ]
  (* TODO: is si=1 correct here? *)
  and body_asm = compile_aexpr body 1 env (List.length args) true in (* Rule 2: the body of a function is always in tail pos *)
    stack_setup_asm
    @ [IInstrComment(ILabel(Printf.sprintf "fun_%s_body" fun_name), Printf.sprintf "Body for %s" fun_name)]
    @ body_asm
    @ postlude_asm;

and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) : instruction list =
  (*(Printf.printf "Compile aexpr (tail=%b): %s\n" is_tail (string_of_aexpr e));*)
  match e with
  | ALet(id, let_e, body, _) ->
     let let_init = compile_cexpr let_e (si + 1) env num_args false in (* Rule 3b: not in tail *)

     (* TODO: Not sure if -4*(si+1) is the right way to calculate this offset, I just made it up but it works *)
     let res_offset = RegOffset(~-4*(si+1), EBP) in

     let body = compile_aexpr body (si + 1) ((id, res_offset)::env) num_args is_tail in  (* Rule 3a: may be in tail *)

     [ILineComment(Printf.sprintf "Let id: %s to store in %s" id (arg_to_asm(res_offset)))]
     @ let_init
     @ [IInstrComment(IMov(res_offset,  Reg(EAX)), (Printf.sprintf "Save variable '%s'" id))] (* TODO: is offset wrong? I don't think so *)
     @ [ILineComment(Printf.sprintf "Let body: %s" (string_of_aexpr e))]
     @ body
     @ [ILineComment("End of let body")]

  | ACExpr(ace_e) ->
      compile_cexpr ace_e si env num_args is_tail (* Just pass is_tail along *)


and compile_cexpr (e : tag cexpr) si env num_args is_tail =
  match e with
  | CPrim1(op, e, _) ->
    let e_reg = compile_imm e env in
      [IMov(Reg(EAX), e_reg)] @
          (match op with
           | Add1   -> intcheck @ [IAdd(Reg(EAX), HexConst(2))] @ overflowcheck
           | Sub1   -> intcheck @ [ISub(Reg(EAX), HexConst(2))] @ overflowcheck
           | Print  -> [IPush(Reg(EAX)); ICall("print"); IAdd(Reg(ESP), HexConst(4))]
           | PrintB -> [IPush(Reg(EAX)); IXor(Reg(EAX), bool_mask); ICall("print"); IAdd(Reg(ESP), HexConst(4))]

           | IsBool ->
               [IAnd(Reg(EAX), HexConst(0x1)); IShl(Reg(EAX), HexConst(31)); IXor(Reg(EAX), const_false)]

           | IsNum  ->
               [IAnd(Reg(EAX), HexConst(0x1)); IShl(Reg(EAX), HexConst(31)); IXor(Reg(EAX), const_true)]

           | Not  ->
               boolcheck "logic" @ [IXor(Reg(EAX), HexConst(0x80000000))]

           | PrintStack -> failwith "Not yet implemented" (* TODO: extra credit? *)
      )
  | CPrim2(op, left, right, tag) ->
    (* Copy left into EAX, do operation with right on stack. Store output in eax *)
    let left_val = compile_imm left env in
      let right_val = compile_imm right env in
        let end_label = cmp_label op tag in (* build a label if necessary for conditional branches *)

        let prefix, suffix = (typechecks left_val right_val op) in
        let body =
            (match op with
             | And  ->  [ IMov(Reg(EAX), left_val);    (* Put left into eax *)
                          IAnd(Reg(EAX), right_val)]   (* Compare right on stack with eax *)

             | Or  ->  [ IMov(Reg(EAX), left_val);   (* Put left into eax *)
                          IOr(Reg(EAX), right_val)]   (* Compare right on stack with eax *)


            (* Logical operations *)
             | Greater  -> compare_vals left_val right_val (IJg(end_label))  end_label
             | GreaterEq-> compare_vals left_val right_val (IJge(end_label)) end_label
             | Less     -> compare_vals left_val right_val (IJl(end_label))  end_label
             | LessEq   -> compare_vals left_val right_val (IJle(end_label)) end_label
             | Eq | EqB -> compare_vals left_val right_val (IJe(end_label))  end_label

             (* Math operations *)
             | Plus  -> [IMov(Reg(EAX), left_val); IAdd(Reg(EAX), right_val)]
             | Minus -> [IMov(Reg(EAX), left_val); ISub(Reg(EAX), right_val)]
             | Times -> (* Shift one argument right (temporarily not a Number in our runtime)
                              so after the multiply, it will be the right runtime value*)
                        [IMov(Reg(EAX), left_val); ISar(Reg(EAX), HexConst(1)); IMul(Reg(EAX), right_val)]

            )
        in [ILineComment(Printf.sprintf "Start of prim2: '%s' " (name_of_op2 op))]
           @ prefix @ body @ suffix
           @ [ILineComment(Printf.sprintf "End of prim2 '%s'" (name_of_op2 op))];

   | CIf(cond, thn, els, tag) ->
     let else_label = sprintf "$if_%d_else" tag in
       let end_label = (sprintf "$if_%d_end" tag) in
       [IMov(Reg(EAX), (compile_imm cond env))] @      (* Evaluate cond, store result in EAX *)
        boolcheck "if" @                        (* Runtime check to ensure cond is a bool *)
        [ ICmp(Reg(EAX), const_false);          (* Test if cond is not true *)
          IJe(else_label) ] @                   (* If it isn't true, jmp to else *)
        (compile_aexpr thn si env num_args is_tail) @  (* Then: body branch *) (* Rule 4b: may be in tail pos *)
        [ IJmp(end_label);                      (* Then: jump to end *)
          ILabel(else_label) ] @                (* Else: label *)
        (compile_aexpr els si env num_args is_tail) @  (* Else: body *) (* Rule 4b: may be in tail pos *)
        [ILabel(end_label)]                     (* End label *)

   | CApp(name, exprs, _) ->
       (* Available vars:  si env num_args is_tail *)
       (* For each expr, push onto stack, then call *)
       if is_tail=true && num_args=(List.length exprs) then
         (
        
         [ILineComment(Printf.sprintf "Prepare for tailcall to function fun_%s" name); ]

         (* Push update the stack (below ebp) with our new arguments *)
         @ (replace_args exprs env)
         @ [ IJmp(Printf.sprintf "fun_%s_body" name)]


        )
       else (
         (* (Printf.printf "Non-tail call to %s (tail=%b, num_args incoming=%d, about to pass %d args)\n" name is_tail num_args (List.length exprs)); *)
         [ILineComment(Printf.sprintf "Prepare to call function fun_%s" name); ] @
         (* Push arguments onto stack  *)
         flatten (List.map(fun x ->
           [ IMov(Reg(EAX), compile_imm x env ); IInstrComment(IPush(Reg(EAX)), (Printf.sprintf "Argument %s" (string_of_immexpr x))) ] (* Copy off memory into EAX, then push EAX *)
           ) exprs) (* Push args onto the stack *)

         (* The stack is setup, now call the function, then cleanup stack pointer*)
         @ [ ICall(Printf.sprintf "fun_%s" name)]
         @ [ IAdd(Reg(ESP), HexConst(4*(List.length exprs))) ] (* Reset stack pointer after call *)
         )

   | CImmExpr(imm) ->
       [ IMov(Reg(EAX), compile_imm imm env)]

and compile_imm e env : arg = 
  match e with
  | ImmNum(n, _) -> HexConst((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)

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
      [ IPop(Reg(EAX)); (* Pop old value off stack into Eax *)
       IInstrComment(IMov(RegOffset(4*(idx+1), EBP), Reg(EAX)), (Printf.sprintf "Argument %s (idx %d)" (string_of_immexpr head) idx)) ]
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
        let this_res = [(vname, RegOffset(offset, EBP))] in (* Params are EBP relative *)
        (* (Printf.printf "Building env: %s -> EBP+%d\n" vname offset); *)
        (_build_env tail (parsed @ this_res)
        )
    | _ -> ([], parsed)
  in let (_, result) = _build_env vars [] in
    result

let rec compile_decl (prog_decls: tag adecl list) : instruction list =
  match prog_decls with
  | ADFun(fun_name, vars, body, loc)::tail ->
      (* Vars is a list of varnames, use as our env and map them to stack offsets, then compile_fun *)
      let local_env = build_env vars in
      (compile_fun fun_name body vars local_env false) (* compile_fun will call compile_aexpr with is_tail=True *)
      @ compile_decl tail (* Recurse for next fn *)
  | _ -> []

let compile_prog (anfed : tag aprogram) : string =
  let (prog_decls, prog_expr, prog_loc) =
    (match anfed with
     | AProgram(decls, exprs, loc) -> (decls, exprs, loc)
    )
  and prelude =
  "section .text
extern error
extern print
;; Error labels are explicitly globals to aid with debugging in gdb
global fun_f
global fun_f_body
global err_arith_numb
global err_comp_numb
global err_overflow
global err_if_bool
global err_logic_bool
global our_code_starts_here\n" in (* TODO: remove f *)


  (* Create functions from list *)
  let functions_asm = [ILineComment("Function defs")] @ (compile_decl prog_decls)
  (* Main expr: use compile_fun with a label of 'our_code_starts_here' on prog_expr *)
  and main_asm = (compile_fun "our_code_starts_here"  prog_expr [] [] true)

  (* Labels for errors *)
  and postlude_asm = [
      ILabel("err_arith_numb"); (* arithmetic expected a number *)
        ISub(Reg(ESP), HexConst(4)); IPush(HexConst(0x0)); ICall("error");

      ILabel("err_comp_numb"); (* comparison expected a number *)
        ISub(Reg(ESP), HexConst(4)); IPush(HexConst(0x1)); ICall("error");

      ILabel("err_overflow"); (* overflow *)
        ISub(Reg(ESP), HexConst(4)); IPush(HexConst(0x2)); ICall("error");

      ILabel("err_if_bool"); (* if expected a bool *)
        ISub(Reg(ESP), HexConst(4)); IPush(HexConst(0x3)); ICall("error");

      ILabel("err_logic_bool"); (* logic expected a bool *)
        ISub(Reg(ESP), HexConst(4)); IPush(HexConst(0x4)); ICall("error");
    ] in

  let all_asm = functions_asm @ main_asm @ postlude_asm in
    prelude ^
    (List.fold_left (fun acc ins -> (acc ^ (i_to_asm ins) ^ "\n")) "" all_asm)




(* Add a desugaring phase somewhere in here, as well as your typechecker *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (add_err_phase well_formed is_well_formed)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename_and_tag)
  |> (add_phase anfed (fun p -> atag (anf p)))
  |> (add_phase result compile_prog)
;;
