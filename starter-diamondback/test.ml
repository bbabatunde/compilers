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



let regression_tests = [

  t "integration1" "if false : 5 else: 6" "6";
  t "integration3" "1" "1";
  t "integration4" "-1" "-1";
  t "integration5" "add1(2)" "3";
  t "integration6" "sub1(2)" "1";
  t "integration7" "add1(sub1(3))" "3";
  t "integration9" "(4 + 5)" "9";
  t "integration11" "(6 - 7)" "-1";
  t "integration13" "(8 * 9)" "72";
  t "integration14" "(3 + 4) * (5 + 6)" "77";
  t "integration15a" "let x = true, y = 4 in if x : 10 else: 20" "10";
  t "integration15b" "let x = 3, y = 4 in y" "4";
  t "integration15c" "let x = 3, y = 4 in add1(y)" "5";
  t "integration15d" "let x = 3, y = 4 in y + x" "7";
  t "integration16" "let x = 3, y = 5 + 2 in x * y" "21";
  t "integration17" "let q = 4, r = let s = 1, t = 3 in t - s in q * r" "8";
  t "integration18" "add1((3+3))" "7";
  t "integration19" "add1((let z = 5 in z))" "6"; 
  t "integration22" "let x = 3, y = 9 in if let z = true in isbool(z) : x * y else: x + y" "27";
  t "integration23" "(3 * -2)" "-6";
  t "integration24" "(-3 * -2)" "6";
   

]

let prim1_tests = [

   t "isbool_true1" " isbool(true)" "true";
   t "isbool_true2"  "isbool(false)" "true";
   t "isbool_false1" "isbool(1)"    "false";
   t "isbool_multiple1" "isbool(isbool(true))" "true";
   t "isbool_multiple2" "isbool(isbool(1))" "true";

   t "isnum_true1" "isnum(1)" "true";
   t "isnum_false1" "isnum(isnum(true))" "false";
   t  "isnum2" "isnum(print(true))" "true\nfalse";



   t "add11" "add1(1)" "2";
   t "add15" "add1(-2)" "-1";
   te "add12" "add1(true)"  "arithmetic expected a number, but got 00ffffffff";
   te "add13" "add1(false)"  "arithmetic expected a number, but got 007fffffff";
   te "add1overflow1" "add1(1073741823)" "overflow value 0080000000";

   te "sub1_1" "sub1(-1073741824)" " overflow value 007ffffffe";

   t "print_1" "print(1)" "1\n1";
   t "print_let" "let x = print(40) in x" "40\n40";
   t "print_let2" "let x = 1 in let y = print(x + 1) in print(y + 2)" "2\n4\n4";
   t "printadd1" "add1(add1(print(4)))" "4\n6";
   t "printadd12" "print(add1(add1(0)))" "2\n2";

   t  "not_1" "!(false)" "true";
   t  "not_2" "!(true)" "false";
   t  "not_8" "!(print(true))" "true\nfalse";
   t  "not_6" "!(!(false))" "false";
   te "not3" "!(1)" "logic expected a boolean, but got 1";
   te "not4" "!(-1)" "logic expected a boolean, but got -1";
   te "not5" "!(!(-1))" "logic expected a boolean, but got -1";


]

let prim2_test = [

   t "plus_1" "(1 + 1)" "2";
   t "plus_3" "(1 + 1) + (2 + 4)" "8";
   t "printplus1" "print(1+1)" "2\n2";
   
   t "sub_1" "(1 - 1)" "0";
   t "sub_2" "(1 - 1) + (2 - 3)" "-1";
   t "subprint2" "(print(1) - 1)" "1\n0";
   t "subprint3" "(print(1) - print(1))" "1\n1\n0";
   t "subprint4" "print(print(1) - print(1))" "1\n1\n0\n0";

   t "times_1" "(3 * 2)" "6";
   t "times_2" "2 * (-1)" "-2";
   t "timesprint2" "(print(1) * 1)" "1\n1";
   t "timesprint3" "(print(1) * print(1))" "1\n1\n1";
   t "timesprint4" "print(print(1) * print(1))" "1\n1\n1\n1";
   t "timesadd11" "(add1(3) * sub1(3))" "8";


   t "and_1" "(true && true)" "true";
   t "and_2" "(true && false)" "false"; 
 
   t "and_8" "(print(false) && print(false))" "false\nfalse\nfalse";
   te "and_4" "(1 && false)" "logic expected a boolean, but got 1";
   te "and_5" "(false && 1)" "logic expected a boolean, but got 1";
   te "and_6" "(1 && 1)" "logic expected a boolean, but got 1";

   t "or_1" "(true || true)" "true";
   t "or_2" "(true || false)" "true"; 
   t "or_3" "(false || false)" "false";
   te "or_4" "(1 || false)" "logic expected a boolean, but got 1";
   te "or_5" "(false || 1)" "logic expected a boolean, but got 1";

   t "greater_1" "(1 > 0)" "true";
   t "greater_2" "(3 > 2)" "true"; 
   t "greater_3" "(-1 > 10)" "false";
   t "greater_6" "(1 > 1)" "false";
   te "greater_4" "(1 > true)" "comparison expected a number, but got 00ffffffff";
   te "greater_5" "(false > 1)" "comparison expected a number, but got 007fffffff";

   t "greateq_1" "(1 >= 1)" "true";
   t "greateq_2" "(3 >= 2)" "true"; 
   t "greateq_3" "(-1 >= -1)" "true";
   t "greateq_6" "(100 >= 100)" "true";

   te "greateq_4" "(1 >= true)"  "comparison expected a number, but got 00ffffffff";
   te "greateq_5" "(false >= 1)" "comparison expected a number, but got 007fffffff";

   t "less1" "(1 < 0)" "false";
   t "less2" "(3 < 2)" "false"; 
   t "less3" "(-1 < 10)" "true";
   t "less6" "(1 < 1)" "false";
   te "less4" "(1 < true)"  "comparison expected a number, but got 00ffffffff";
   te "less5" "(false < 1)" "comparison expected a number, but got 007fffffff";

   t "lesseq_1" "(1 <= 1)" "true";
  
   te "lesseq_4" "(1 >= true)" "comparison expected a number, but got 00ffffffff";
   te "lesseq_5" "(false >= 1)" "comparison expected a number, but got 007fffffff";
   t "lesseq_6" "(100 >= 100)" "true";

   t "eq_1" "(1 == 0)" "false";
   t "eq_2" "(3 == 2)" "false";
   t "eq_3"  "(print(1) == print(true))"  "1\ntrue\nfalse"; 
   t "eq_4" "(1 == true)"  "false";


]

let if_tests = [
  
   te "if_1" "if 54: true else: false" "if expected a boolean, but got 54";
   t "if_2" "if true: 1 else: 2" "1";
   t "if_3" "if false: 1 else: print(2)" "2\n2";
   t "if_5" "if print(false): 1 else: print(2)" "false\n2\n2";
]


let tests = [ 
  tanf "isanf1" (Program([],(ENumber(1,())),()))  (AProgram([],(ACExpr(CImmExpr(ImmNum(1,())))),())); 
  t "integration1" "add1(add1(7))" "9";
  te "integration2" "let x= 1, x = 2 in x"  "The identifier x, redefined at <integration2, 1:10-1:11>, duplicates one at <integration2, 1:4-1:5";
  te "integration3" "let x = 1, y = 2, z = 3 in let x = 1 in x + y + z "  "The identifier x, defined at <integration3, 1:31-1:32>, shadows one defined at <integration3, 1:4-1:5>\nThe identifier x, redefined at <integration3, 1:31-1:32>, duplicates one at <integration3, 1:4-1:5>";
  te "integration4" "add1(1073741823)" "overflow value 0080000000";
  te "integration6" "add1(x)" "The identifier x, used at <integration6, 1:5-1:6>, is not in scope";
  te "integration7"  "def f(x, x):\n y\n d(1)" "The identifier y, used at <integration7, 2:1-2:2>, is not in scope\nThe identifier x, redefined at <integration7, 1:9-1:10>, duplicates one at <integration7, 1:9-1:10>\nThe function name d, used at <integration7, 3:1-3:5>, is not in scope";
  t "integration9"  "def f(a,b,c): (1 * c) f(1,2,8)" "5";
  t "integration10"  "def f(): (1 * 100) f()" "100";
  te "integration11"  "def f(): (1 * 100) def f(a,b): (a + b) f()" "The function name f, redefined at <integration11, 1:0-1:18>, duplicates one at <integration11, 1:19-1:38>\nThe function name f, redefined at <integration11, 1:19-1:38>, duplicates one at <integration11, 1:19-1:38>\nThe function name f, redefined at <integration11, 1:0-1:18>, duplicates one at <integration11, 1:19-1:38>";



]


let suite =	
"suite">:::
  prim1_tests @
  prim2_test @
  if_tests @
  regression_tests @
  tests @
  []
 


let () =
  run_test_tt_main suite
;;

