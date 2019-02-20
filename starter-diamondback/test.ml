open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Errors

let is_osx = Conf.make_bool "osx" true "Set this flag to run on osx";;

let fallable_printer p: string =
	match p with
	|Ok(e) -> string_of_program e
	|Error(lst) -> String.concat " " (List.map (fun e -> e ) (print_errors lst));;

let string_to_lexingpost e  =
 let lexbuf =  Lexing.from_string e in 
  ((Lexing.lexeme_start_p lexbuf) , (Lexing.lexeme_end_p lexbuf)) ;; 

let twellformed  name program expected = name>::fun _ -> assert_equal program expected ~printer:fallable_printer;;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tvg name program expected = name>::test_run_valgrind program name expected;;
  
let tanf name program expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;



let tests = [ 
  tanf "isanf1" (Program([],(ENumber(1,())),()))  (AProgram([],(ACExpr(CImmExpr(ImmNum(1,())))),())); 
  (*tanf "isanf2" (Program([DFun("f",[("x",());("y",())],(EPrim1(Add1,EId("x",()),())),())], 
  	(EApp("f", [(ENumber(1,()));(ENumber(1,()))], ())),())) (AProgram([],(ACExpr(CImmExpr(ImmNum(1,())))),())); 
  twellformed "twellformed1" (is_well_formed(parse_string ""  "def fact_v1(n) : if n <= 1: 1 else: n * fact_v1(n - 1)")) (Error([Overflow(1, string_to_lexingpost "add1(1073741828)")]));*)
  (* twellformed "twellformed2" (is_well_formed(parse "" (Lexing.from_string "let x=1, x =2 in x + 2"))) (Error([Overflow(1073741828,(string_to_lexingpost ""))]));*)
  (*twellformed "twellformed2" (is_well_formed(parse_string "" "let x = 1, y = 2, z = 3 in let x = 1 in x + y + z ")) (Error([Overflow(1073741828,(string_to_lexingpost ""))]));*)
 
 (*  (Ok(Program([],
  	(ENumber(1,((Lexing.lexeme_start_p (Lexing.from_string "1")), (Lexing.lexeme_end_p (Lexing.from_string "1"))))),

  	((Lexing.lexeme_start_p (Lexing.from_string "1")), (Lexing.lexeme_end_p (Lexing.from_string "1"))))));  *)

  t "integration1" "sub1(add1(add1(1)))" "2";
  
]

let suite =	
"suite">:::

 tests @
 []
 


let () =
  run_test_tt_main suite
;;

