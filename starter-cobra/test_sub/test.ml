open Compile
open Runner
open Printf
open OUnit2



let t name program expected = name>::test_run program name expected;;
let te name program expected = name>::test_err program name expected;;

let forty = "let x = 40 in x"
let fals = "let x = false in x"
let tru = "let x = true in x"


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
   t "printplus2" "(print(1) + 1)" "1\n2";
   t "printplus3" "(print(1) + print(1))" "1\n1\n2";
   t "printplus4" "print(print(1) + print(1))" "1\n1\n2\n2";
   te "plus4" "(1 + true)" "arithmetic expected a number, but got 00ffffffff";
   te "plus5" "(true + 1)" "arithmetic expected a number, but got 00ffffffff";
   te "plusoverlow" "(1073741820 + 8)" "overflow value 0080000008";

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
   t "and_3" "(false && false)" "false";
   t "and_7" "(print(false) && false)" "false\nfalse";
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
   t "lesseq_2" "(3 <= 2)" "false"; 
   t "lesseq_3" "(-1 <= -1)" "true";
   te "lesseq_4" "(1 >= true)" "comparison expected a number, but got 00ffffffff";
   te "lesseq_5" "(false >= 1)" "comparison expected a number, but got 007fffffff";
   t "lesseq_6" "(100 >= 100)" "true";

   t "eq_1" "(1 == 0)" "false";
   t "eq_2" "(3 == 2)" "false";
   t "eq_3"  "(print(1) == print(true))"  "1\ntrue\nfalse"; 
   t "eq_4" "(1 == true)"  "false";
   t "eq_5" "(false == 1)" "false";
   t "eq_6" "(1 == 1)" "true";
   t "eq_7" "(1 == 1) == (true == true)" "true";
   t "eq_8" "(1 == 1) == (true == false)" "false";
   t "eq_9" "!((1 == 1) == (true == false))" "true";

]

let if_tests = [
  
   te "if_1" "if 54: true else: false" "if expected a boolean, but got 54";
   t "if_2" "if true: 1 else: 2" "1";
   t "if_3" "if false: 1 else: print(2)" "2\n2";
   t "if_5" "if print(false): 1 else: print(2)" "false\n2\n2";
]

let integration_tests = [

   t "forty" forty "40";

   
]

let suite =
"suite">:::
  prim1_tests @
  prim2_test @
  if_tests @
  regression_tests @
  integration_tests 
 
;;


let () =
  run_test_tt_main suite
;;
