open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Assembly
open Errors
       
let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx";;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tvg name program expected = name>::test_run_valgrind program name expected;;
  
let tanf name program expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let forty_one = "41";;

let forty_one_a = (AProgram([], ACExpr(CImmExpr(ImmNum(41, ()))), ()))

let test_prog = "let x : Int = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: (if 0 == 0: sub1(4) else: sub1(5)) in x"
let anf1 = (anf     (tag (parse_string "test" test_prog)))

let suite =
"suite">:::
 [

  tanf "forty_one_anf"
       (Program([], ENumber(41, ()), TyBlank(), ()))
       forty_one_a;

  (* tanf "prim1_anf"
   *      (Program([], (EPrim1(Sub1, ENumber(55, ()), ())), ()))
   *      (AProgram([],
   *                (ALet("unary_1", CPrim1(Sub1, ImmNum(55, ()), ()),
   *                      ACExpr(CImmExpr(ImmId("unary_1", ()))),
   *                      ())),
   *               ())); *)

  te "scope_err1" "let x : Bool = true in (let y : Bool = (let x : Bool = false in x) in y)" "shadows one defined";

  ta "forty_one_run_anf" (atag forty_one_a) "41";
 
  t "forty_one" forty_one "41";


  t "test" test_prog "3";
      
    (* Some useful if tests to start you off *)

  t "if1" "if 7 < 8: 5 else: 3" "5";
  t "if2" "if 0 > 1: 4 else: 2" "2";
  te "integration3" "let x = 1, y = 2, z = 3 in let x = 1 in x + y + z "  "The identifier x, defined at <integration3, 1:31-1:32>, shadows one defined at <integration3, 1:4-1:5>\nThe identifier x, redefined at <integration3, 1:31-1:32>, duplicates one at <integration3, 1:4-1:5>";

  te "overflow" "add1(1073741823)" "overflow";

  t "integration1" "add1(add1(7))" "9";
  te "integration2" "let x= 1, x = 2 in x"  "The identifier x, redefined at <integration2, 1:10-1:11>, duplicates one at <integration2, 1:4-1:5";
  te "integration3" "let x = 1, y = 2, z = 3 in let x = 1 in x + y + z "  "The identifier x, defined at <integration3, 1:31-1:32>, shadows one defined at <integration3, 1:4-1:5>\nThe identifier x, redefined at <integration3, 1:31-1:32>, duplicates one at <integration3, 1:4-1:5>";
  te "integration4" "add1(1073741823)" "overflow value 0080000000";
  te "integration6" "add1(x)" "The identifier x, used at <integration6, 1:5-1:6>, is not in scope";
  te "integration7"  "def f(x, x):\n y\n d(1)" "The identifier y, used at <integration7, 2:1-2:2>, is not in scope\nThe identifier x, redefined at <integration7, 1:9-1:10>, duplicates one at <integration7, 1:9-1:10>\nThe function name d, used at <integration7, 3:1-3:5>, is not in scope";
  
  
  ]
;;


let () =
  run_test_tt_main suite
;;
