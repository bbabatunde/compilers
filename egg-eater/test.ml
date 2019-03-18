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

let tprog filename expected = filename>::test_run_input filename expected;;

let forty_one = "41";;

let forty_one_a = (AProgram([], ACExpr(CImmExpr(ImmNum(41, ()))), ()))

let test_prog = "let x : Int = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: (if 0 == 0: sub1(4) else: sub1(5)) in x"
let anf1 = (anf     (tag (parse_string "test" test_prog)))

let suite =
"suite">:::
 [

   tanf "forty_one_anf"
       (Program([], [], EAnnot(ENumber(41, ()), TyBlank(), ()), ()))
       forty_one_a;

  (* tanf "prim1_anf"
   *      (Program([], (EPrim1(Sub1, ENumber(55, ()), ())), ()))
   *      (AProgram([],
   *                (ALet("unary_1", CPrim1(Sub1, ImmNum(55, ()), ()),
   *                      ACExpr(CImmExpr(ImmId("unary_1", ()))),
   *                      ())),
   *               ())); *)

  (*te "scope_err1" "let x : Bool = true in (let y : Bool = (let x : Bool = false in x) in y)" "shadows one defined";*)

  ta "forty_one_run_anf" (atag forty_one_a) "41";
 
  t "forty_one" forty_one "41";


  t "test1" test_prog "3";
      
    (* Some useful if tests to start you off *)

  t "if1" "if 7 < 8: 5 else: 3" "5";
  t "if2" "if 0 > 1: 4 else: 2" "2";
  t "prim" "(2+2) + (2+2)" "8";
  te "overflow" "add1(1073741823)" "overflow";
  t "funcalls1" "def f(n): n\n\n f(1)" "1"; 
  t "funcalls"  "def fact(n : Int) -> Int: if n < 2: 1 else: n * fact(n - 1)\n\nfact(5)" "120";
  t "tuplepair" "let zero = (100,1,100) in zero[0 of 2]" "100";
  t "tuplepair1" "let t = (3, ((4, true), 5)) in 
                  let (x, (y, z)) = t in
                  x + y[0 of 2] + z" "12";

  t "tuplepair2" "let three = ((4, (true, 3))) in
                  three" "(4, (true, 3))";

  t "tuplepair4" "let three = ((4, (true, 3))) in
                  three" "(4, (true, 3))";
 
  t "tuplepair3" "let three = (1, 2, 3,4) in
                  three" "(1, 2, 3, 4)";
  
  t "tuplepair5" "let two = (1,2,3,4) in two[1 of 5 := 10] "  "(1, 10, 3, 4)";

  t "printtuple" "print((1, 2, 3,4))" "(1, 2, 3, 4)\n(1, 2, 3, 4)";

  t "equalif" "if 1 == 1:  1 else: 0" "1";

  t "and" "(true && true)" "true";

  t "plaineq" "(1 == 1)" "true";

  t "tupleeq1"  "let t = (4, 5) in t == t" "true";

  t "tupleeq2"  "(4,5) == (4, 5)"   "true";

  t "tupleeq3"  "(4,5,6) == (4,5,6)"   "true";

  t "tupleeq4"  "(4,5) == (4,5,7)"   "false";

  t "tupleeq5"  "(4,5,6) == (4,5,7)"   "false";

  t "tupleeq6"  "(4,5,(1,6)) == (4,5,(1,6))"   "true";

  t "tupleeq7"  "(4,5,(1,7)) == (4,5,(1,6))"   "false";

  t "istuple1"   "istuple(1)"  "false";

  t "istuple2"   "istuple((1,2,3) )"  "true";

  t "istuple3"   "istuple((1,2,(2,3)))"  "true";

  (*FIX THIS*)
  (*t "istuple4"   " istuple(nil)" "true";*)

  (* Sequence tests *)  
  t "seq1" "let a = 1; 2; 3; 4; 5 in a" "5";
  t "seq2" "1 + ( 2;3;4;5 )" "6";
  t "seq3" "add1(2;3;4;5)" "6";
  t "seq4" "if(false; true): 1;2;3 else: 2;2;3" "3";
  t "seq5" "if(false; false): 1 else: 2;3;4" "4";
  t "seq6" "def f(n): n
            f(1;2;3)" "3";
  t "seq7" "(1;2;3, 1;2;3)" "(3, 3)";
  (*t "seq8" "(((1;2;3), (3;4)), 1)" "((3, 4), 1)"*)
 
  t "seq8" "(((1;2;3), (3;4)), 1)" "((3, 4), 1)";

  (* Desugar tuple tests *)
  t "desugar1" "let (a, b, c) = (1, 2, 3) in c" "3";
  t "desugar2" "let (a, (b, c)) = (1, (2, 3)) in c" "3";
  t "desugar2_1" "let (a, (b, c)) = (1, (2, 3)) in b" "2";
  t "desugar2_2" "let (a, (b, c)) = (1, (2, 3)) in a" "1";
  t "desugar3" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in d" "4";
  t "desugar3_2" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in c" "3";
  t "desugar3_3" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in b" "2";
  t "desugar3_4" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in a" "1";
  t "desugar4" "let (a, b) = (1, (2, 3)) in b" "(2, 3)";

  (* Nested let bindings *)
  t "nlet1" "let a = (1, 2, 3) in
             let (b, c, d) = a in
             d; c; b" "1";
  t "nlet2" "let a = (1, 2, 3) in
             let (b, c, d) = a in
             let e = (b, c) in
             let (f, g) = e in
             g" "2";
  t "nlet3" "let (a, b) = (1, (2, 3)) in
             let (c, d) = b in
             d" "3";
  t "nlet4" "let a = (1, (2, 3)) in
             a[1 of 2][1 of 2]" "3";

  (* Test sequence of sets *)
  t "set1" "let a = (1,2) in
                a[0 of 2 := 0];
                a[1 of 2 := 0];
                a" "(0, 0)";
  t "set2" "let a = ((1, 2), (3, 4)) in
                a[0 of 2][0 of 2 := 0];
                a[0 of 2][1 of 2 := 0];
                a[1 of 2][0 of 2 := 0];
                a[1 of 2][1 of 2 := 0];
                a" "((0, 0), (0, 0))";

  (* Nested function argument bindings. *)
  t "desugar_fn1" "def f((x1, y1), (x2, y2)): (x1 + x2, y1 + y2)
                   f((1,2), (1, 2))" "(2, 4)";
  t "desugar_fn2" "def f((x, y, z)): x + y + z
                   f((1,1,1))" "3";
  t "desugar_fn3" "def f(): g((1, 2))
                   and def g(a): let (b, c) = a in h(b, c)
                   and def h(v1, v2): v1 + v2
                   f()" "3";


  (* Mixed function desugaring *)
  t "desugar_fn4" "def f((x, y), z): (x + z, y + z)
                   f((1, 2), 3)" "(4, 5)";
  t "desugar_fn5" "def f((a, b), c, (d, e)): a + b + c + d + e
                   f((1, 1), 1, (1, 1))" "5";
  t "desugar_fn6" "def f(a): let (b, c, d, e) = a in b + c + d; e
                   let z = (1, 2, 3, 4) in f(z)" "4";

  (* Test scoped functions with and. *)
  t "fnt1" "def f(): g()
            and def g(): h()
            and def h(): 1
            h();
            g();
            f()" "1";
  te "fnt2" "def f(): g()
             def g(): h()
             and def h(): 1
             h();
             g();
             f()"
             "Scope error";

  (* Test list programs *)
  tprog "lists.egg" "PLACEHOLDER";

  ]
;;


let () =
  run_test_tt_main suite
;;
