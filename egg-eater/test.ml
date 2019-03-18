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


let t name program input expected = name>::test_run [] input program name expected;;
let ta name program input expected = name>::test_run_anf [] input program name expected;;
let tgc name heap_size program input expected = name>::test_run [string_of_int heap_size] input program name expected;;
let tvg name program input expected = name>::test_run_valgrind [] input program name expected;;
let tvgc name heap_size program input expected = name>::test_run_valgrind [string_of_int heap_size] input program name expected;;
let terr name program input expected = name>::test_err [] input program name expected;;
let tgcerr name heap_size program input expected = name>::test_err [string_of_int heap_size] input program name expected;;
let tanf name program input expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;
let te name program input expected_err = name>::test_err [] input program name expected_err;;


let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let tprog filename expected = filename>::test_run_input filename [] "" expected;;


let forty_one = "41";;

let forty_one_a = (AProgram([], ACExpr(CImmExpr(ImmNum(41, ()))), ()))

let test_prog = "let x : Int = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: (if 0 == 0: sub1(4) else: sub1(5)) in x"
let anf1 = (anf     (tag (parse_string "test" test_prog)))

let fun_tests = [
   (*function call tests*)
   (*  t "func_dual_call" "def f(x): let z=10 in z
                        def g(y): f(y)
                        g(1)"  "" "10";

    t "fntail_mutual" "def f(c, v): if (c == 0): v else: f2(c - 1, v + 1)
                         and def f2(c2, v2): if (c2 == 0): v2 else: f(c2 - 1, v2 + 1)
                         f(4, 0)" "" "4";

    t "two_arg_tail" "def f(x,y): if (y==0): x else: g(y, x - 1)
                      and def g(x,y): if (y==0): x else: f(y, x - 1)
                    f(1,1)" "" "1";

    t "many_tail" "def f(a,b,c,d,e): if (a==1): print(b) else: f(b,c,d,e,a)
                    f(0,0,0,1,3)" "" "3\n3";

    t "min" "def min(x, y):
          if x < y: x
            else: min(y, x)
        min(3,2)" "" "2";

    t "many_no_tail" "def f(a,b,c,d,e): 
                        let next = (if a==0: f(b,c,d,e,a) else: 0) in 
                            if (a==1):
                                print(b)
                             else:
                                 next
                    f(0,0,0,1,3)" "" "3\n3";

    t "tail1" "def f(x): if (x == 0): x else: f(x - 1) f(100000)" "" "0";

    t "tail0" "def f(x): if (x == 0): x else: f(x - 1)
    f(2)" "" "0"; *)

    (* t "multiple_unused_function" 
        "def f0(): 0
         def f1(): 1
         def f2(): 2
         def f3(): 3
         f2()" "" "2";

    t "fall_through_functions"
        "def f0(): 0
         def f1(): f0()
         def f2(): f1()
         def f3(): f2()
         f3()" "" "0";

    te "function_redefiniton"
        "def f0(): 0
         def f0(): 1
         f0()" "" "The function name f0, redefined at <function_redefiniton, 2:9-2:20>, duplicates one at <function_redefiniton, 1:0-1:11>";

    t "nottail" "def fact_v1(n):
        let x = 
            (if n > 1: fact_v1(n - 1)
            else: 0
            ) in
          if n <= 1: 1
            else: n * x

       fact_v1(4)" "" "24";

    t  "foo" "def g(x, y): x
    def f(x, z):
        let innery = (x - 1) in
            g(innery, innery)
      f(2, 0)" "" "1";
 *)
  (* Tail tests: note we don't use valgrind here because it will complain about modifying our parent's stackframe *)
    t "tail1" "def f(counter,val): if (counter == 0): val else: f(counter - 1, 1) f(1, 2)" "" "1";
   
    (*
    t "func" "def f(x): x  f(1)" "" "1";
    t "func2" "def f(x,y): x + (2*y) f(1,3)" "" "7";
    t "func3" "def f(x):\nx*2\n\nf(41)" "" "82";

    t "funcs1" "def f(x): x + 1\n  def g(y): y - 1\n  g(f(1))" "" "1";

    t "func_rec" "def f(x): x f(f(1))" "" "1"; 

    t "func_fib" "def fib(x): if x > 1: fib(x- 1) + fib(x- 2) else: 1 fib(6)" "" "13";

   
    (* Static errors *)
    te "var_func_arity_e" "def x(y): 1
                    let x = 1 in x()" ""
                    "The function called at <var_func_arity_e, 2:33-2:36> expected an arity of 1, but received 0 arguments";

    te "var_func" "def y(): 1
                let x = 1 in y" ""
                "The identifier y, used at <var_func, 2:29-2:30>, is not in scope";
    t "bad_order_1" "def f(x, y): x
                     def g(x, y): y
                     def h(x, y): 1
                    h(0, 0)" "" "1";
 
    t "fn_base" "def f(): 1
                 f()" "" "1";

    t "two_fns" "def f(n): n
                 def g(): 1
                 f(g())" "" "1";

    te "arity" "def f(x,y): x
                f(1)" "" "The function called at <arity, 2:16-2:20> expected an arity of 2, but received 1 arguments";

    te "arity_nest2" "def f(x): x
                     f(f(1), 0)" "" "The function called at <arity_nest2, 2:21-2:31> expected an arity of 1, but received 2 arguments";

    te "unbound_fun" "f(1)" "" "The function name f, used at <unbound_fun, 1:0-1:4>, is not in scope";

   
    te "duplicate_id_fun" "def f(x,x): x\nf(1,2)" "" "The identifier x, redefined at <duplicate_id_fun, 1:6-1:7>, duplicates one at <duplicate_id_fun, 1:6-1:7>";
    te "duplicate_fun" "def f(x,y): x
                        def f(y,z): y
                        f(1,2)" "" "";
   
    te "multi_error" "def f(x, x): y
                       f(1)" ""
                       ""; *)
  
]

let prim1_tests = [

   t "isbool_true1" " isbool(true)" "" "true";
   t "isbool_true2"  "isbool(false)" "" "true";
   t "isbool_false1" "isbool(1)"  ""  "false";
   t "isbool_multiple1" "isbool(isbool(true))" "" "true";
   t "isbool_multiple2" "isbool(isbool(1))" "" "true";

   t "isnum_true1" "isnum(1)" "" "true";
   t "isnum_false1" "isnum(isnum(true))" "" "false";
   t  "isnum2" "isnum(print(true))" "" "true\nfalse";



   t "add11" "add1(1)" "" "2";
   t "add15" "add1(-2)" "" "-1";
   te "add12" "add1(true)" "" "arithmetic expected a number, but got 00ffffffff";
   te "add13" "add1(false)" "" "arithmetic expected a number, but got 007fffffff";
   te "add1overflow1" "add1(1073741823)" "" "overflow value 0080000000";

   te "sub1_1" "sub1(-1073741824)" "" "overflow value 007ffffffe";

   t "print_1" "print(1)" "" "1\n1";
   t "print_let" "let x = print(40) in x" "" "40\n40";
   t "print_let2" "let x = 1 in let y = print(x + 1) in print(y + 2)" "" "2\n4\n4";
   t "printadd1" "add1(add1(print(4)))" "" "4\n6";
   t "printadd12" "print(add1(add1(0)))" "" "2\n2";

   t  "not_1" "!(false)" "" "true";
   t  "not_2" "!(true)" "" "false";
   t  "not_8" "!(print(true))" "" "true\nfalse";
   t  "not_6" "!(!(false))" "" "false";
   te "not3" "!(1)" "" "logic expected a boolean, but got 1";
   te "not4" "!(-1)" "" "logic expected a boolean, but got -1";
   te "not5" "!(!(-1))" "" "logic expected a boolean, but got -1";


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
  
   te "if_1" "if 54: true else: false" "" "if expected a boolean, but got 54";
   t "if_2" "if true: 1 else: 2" "" "1";
   t "if_3" "if false: 1 else: print(2)" "" "2\n2";
   t "if_5" "if print(false): 1 else: print(2)" "" "false\n2\n2";
]

let integration_tests = [

   tanf "forty_one_anf"
       (Program([], [], EAnnot(ENumber(41, ()), TyBlank(), ()), ())) ""
       forty_one_a;

  (* tanf "prim1_anf"
   *      (Program([], (EPrim1(Sub1, ENumber(55, ()), ())), ()))
   *      (AProgram([],
   *                (ALet("unary_1", CPrim1(Sub1, ImmNum(55, ()), ()),
   *                      ACExpr(CImmExpr(ImmId("unary_1", ()))),
   *                      ())),
   *               ())); *)

  (*te "scope_err1" "let x : Bool = true in (let y : Bool = (let x : Bool = false in x) in y)" "shadows one defined";*)

  ta "forty_one_run_anf" (atag forty_one_a) "" "41";
 
  t "forty_one" forty_one "" "41";


  t "test1" test_prog "" "3";
      
    (* Some useful if tests to start you off *)

  t "if1" "if 7 < 8: 5 else: 3" "" "5";
  t "if2" "if 0 > 1: 4 else: 2" "" "2";
  t "prim" "(2+2) + (2+2)" "" "8";
  te "overflow" "add1(1073741823)" "" "overflow";
  t "funcalls1" "def f(n): n\n\n f(1)" "" "1"; 
  t "funcalls"  "def fact(n : Int) -> Int: if n < 2: 1 else: n * fact(n - 1)\n\nfact(5)" "" "120";
  t "tuplepair" "let zero = (100,1,100) in zero[0 of 2]" "" "100";
  t "tuplepair1" "let t = (3, ((4, true), 5)) in 
                  let (x, (y, z)) = t in
                  x + y[0 of 2] + z" "" "12";

  t "tuplepair2" "let three = ((4, (true, 3))) in
                  three" "" "(4, (true, 3))";

  t "tuplepair4" "let three = ((4, (true, 3))) in
                  three" "" "(4, (true, 3))";
 
  t "tuplepair3" "let three = (1, 2, 3,4) in
                  three" "" "(1, 2, 3, 4)";
  
  t "tuplepair5" "let two = (1,2,3,4) in two[1 of 5 := 10] " "" "(1, 10, 3, 4)";

  t "printtuple" "print((1, 2, 3,4))" "" "(1, 2, 3, 4)\n(1, 2, 3, 4)";

  t "equalif" "if 1 == 1:  1 else: 0" "" "1";

  t "and" "(true && true)" "" "true";

  t "plaineq" "(1 == 1)" "" "true";

  t "tupleeq1"  "let t = (4, 5) in t == t" "" "true";

  t "tupleeq2"  "(4,5) == (4, 5)" ""  "true";

  t "tupleeq3"  "(4,5,6) == (4,5,6)" ""  "true";

  t "tupleeq4"  "(4,5) == (4,5,7)"  "" "false";

  t "tupleeq5"  "(4,5,6) == (4,5,7)"  "" "false";

  t "tupleeq6"  "(4,5,(1,6)) == (4,5,(1,6))" ""  "true";

  t "tupleeq7"  "(4,5,(1,7)) == (4,5,(1,6))" ""  "false";

  t "istuple1"   "istuple(1)"  "" "false";

  t "istuple2"   "istuple((1,2,3))" "" "true";

  t "istuple3"   "istuple((1,2,(2,3)))" "" "true";

  t "istuple6"   "istuple(nil : Int) == true" "" "true";

  (* Sequence tests *)  
  t "seq1" "let a = 1; 2; 3; 4; 5 in a" "" "5";
  t "seq2" "1 + ( 2;3;4;5 )" "" "6";
  t "seq3" "add1(2;3;4;5)" "" "6";
  t "seq4" "if(false; true): 1;2;3 else: 2;2;3" "" "3";
  t "seq5" "if(false; false): 1 else: 2;3;4" "" "4";
  t "seq6" "def f(n): n
            f(1;2;3)" "" "3";
  t "seq7" "(1;2;3, 1;2;3)" "" "(3, 3)";
  (*t "seq8" "(((1;2;3), (3;4)), 1)" "((3, 4), 1)"*)
 
  t "seq8" "(((1;2;3), (3;4)), 1)" "" "((3, 4), 1)";

  (* Desugar tuple tests *)
  t "desugar1" "let (a, b, c) = (1, 2, 3) in c" "" "3";
  t "desugar2" "let (a, (b, c)) = (1, (2, 3)) in c" "" "3";
  t "desugar2_1" "let (a, (b, c)) = (1, (2, 3)) in b" "" "2";
  t "desugar2_2" "let (a, (b, c)) = (1, (2, 3)) in a" "" "1";
  t "desugar3" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in d" "" "4";
  t "desugar3_2" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in c" "" "3";
  t "desugar3_3" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in b" "" "2";
  t "desugar3_4" "let (a, (b, (c, d))) = (1, (2, (3, 4))) in a" "" "1";
  t "desugar4" "let (a, b) = (1, (2, 3)) in b" "" "(2, 3)";

  (* Desugar more *)
  t "desugar5" "def f(a): a
                let (b, c) = f((1, 2)) in (b, c)" "" "(1, 2)";
  (* Test the above with a fn that returns something on each call to test for temp var desugar stuff. *)

  (* Nested let bindings *)
  t "nlet1" "let a = (1, 2, 3) in
             let (b, c, d) = a in
             d; c; b" "" "1";
  t "nlet2" "let a = (1, 2, 3) in
             let (b, c, d) = a in
             let e = (b, c) in
             let (f, g) = e in
             g" "" "2";
  t "nlet3" "let (a, b) = (1, (2, 3)) in
             let (c, d) = b in
             d" "" "3";
  t "nlet4" "let a = (1, (2, 3)) in
             a[1 of 2][1 of 2]" "" "3";

  (* Test sequence of sets *)
  t "set1" "let a = (1,2) in
                a[0 of 2 := 0];
                a[1 of 2 := 0];
                a" "" "(0, 0)";
  t "set2" "let a = ((1, 2), (3, 4)) in
                a[0 of 2][0 of 2 := 0];
                a[0 of 2][1 of 2 := 0];
                a[1 of 2][0 of 2 := 0];
                a[1 of 2][1 of 2 := 0];
                a" "" "((0, 0), (0, 0))";

  (* Nested function argument bindings. *)
  t "desugar_fn1" "def f((x1, y1), (x2, y2)): (x1 + x2, y1 + y2)
                   f((1,2), (1, 2))" "" "(2, 4)";
  t "desugar_fn2" "def f((x, y, z)): x + y + z
                   f((1,1,1))" "" "3";

  t "desugar_fn3" "def f(): g((1, 2))
                   and def g(a): let (b, c) = a in h(b, c)
                   and def h(v1, v2): v1 + v2
                   f()" "" "3";

  (* Mixed function desugaring *)
  (* t "desugar_fn4" "def f((x, y), z): (x + z, y + z)
                   f((1, 2), 3)" "(4, 5)"; *)
  t "desugar_fn5" "def f((a, b), c, (d, e)): a + b + c + d + e
                   f((1, 1), 1, (1, 1))" "" "5";
  t "desugar_fn6" "def f(a): let (b, c, d, e) = a in b + c + d; e
                   let z = (1, 2, 3, 4) in f(z)" "" "4";

  (* Test scoped functions with and. *)
  t "fnt1" "def f(): g()
            and def g(): h()
            and def h(): 1
            h();
            g();
            f()" "" "1";
  te "fnt2" "def f(): g()
             def g(): h()
             and def h(): 1
             h();
             g();
             f()"
             "" "The function name g, used at <fnt2, 1:9-1:12>, is not in scope";

  (* Testing tail-recursion.*)
    t "tail_rec_len" "def link(first, rest):
                   (first, rest)
                 def length(l):
                   if l == false : 0
                   else:
                    2 + length(l[1 of 2])
                 let mylist = link(1, (link(2, (link(3, false))))) in
                   length(mylist)" "" "10";

  (* Placeholder lists tests. 
  tprog "listsAppend.egg" "";
  tprog "listsLength.egg" "";
  tprog "listsSum.egg" "";
  tprog "listsReverse.egg" "";*)
  
  ]

let infer_tests = [
    t "infer_let_1" "let x=1 in x" "" "meh";
    ]

let suite =
"suite">:::
 infer_tests @
 []
;;


let () =
  run_test_tt_main suite
;;
