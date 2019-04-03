open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors

let t name program input expected = name>::test_run [] input program name expected;;
let ta name program input expected = name>::test_run_anf [] input program name expected;;
let tgc name heap_size program input expected = name>::test_run [string_of_int heap_size] input program name expected;;
let tvg name program input expected = name>::test_run_valgrind [] input program name expected;;
let tvgc name heap_size program input expected = name>::test_run_valgrind [string_of_int heap_size] input program name expected;;
let terr name program input expected = name>::test_err [] input program name expected;;
let tgcerr name heap_size program input expected = name>::test_err [string_of_int heap_size] input program name expected;;
let tanf name program input expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let tfvs name program expected = name>::
  (fun _ ->
    let ast = parse_string name program in
    let anfed = anf (tag ast) in
    let vars = free_vars_P anfed [] in
    let c = Pervasives.compare in
    let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in
    assert_equal (List.sort c vars) (List.sort c expected) ~printer:str_list_print)
;;

let builtins_size = 0

let pair_tests = [
  t "tup1" "let t = (4, (5, 6)) in
            begin
              t[0 of 2 := 7];
              t
            end" "" "(7, (5, 6))";
  t "tup2" "type intlist = (Int * intlist)
            let t : intlist = (4, (5, nil : intlist)) in
            begin
              t[1 of 2 := nil : intlist];
              t
            end" "" "(4, nil)";
  t "tup3" "type intlist = (Int * intlist)
            let t : intlist = (4, (5, nil : intlist)) in
            begin
              t[1 of 2 := t];
              t
            end" "" "(4, <cyclic tuple 1>)";
  t "tup4" "let t = (4, 6) in
            (t, t)"
           ""
           "((4, 6), (4, 6))"

]

let oom = [
  tgcerr "oomgc1" (2 + builtins_size) "(1, (3, 4))" "" "Allocation error: needed 3 words, but the heap is only 2 words";
  tgc "oomgc2" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))";
  tgc "oomgc3" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))";
  tgc "oomgc4" (4 + builtins_size) "(3, 4)" "" "(3, 4)";
  tgcerr "oomgc5" (3 + builtins_size) "(3, 4, 5, 6, 7, 8, 9)" "" "Allocation";

]

let gc = [
  tgc "gc_lam1" (10 + builtins_size)
      "let f = (lambda: (1, 2)) in
       begin
         f();
         f();
         f();
         f()
       end"
      ""
      "(1, 2)";

  tgc "gctest2" 10 "(1, 2)" "" "(1, 2)";

]

let new_gc_tests = [
    (* The only interesting test cases are sequences. *)
    tgcerr "oom1" 9 "(1, 2)" "" "Out of memory";
    tgcerr "oom2" 10 "(1, 2, 3, 4)" "" "Out of memory";

    tgc "seq1" 10 "let a = (1, 2) in 
                    a[1 of 2 := 0];
                    a" "" "(1, 0)";
    tgc "seq2" 10 "let a = (1, 2) in
                   a[1 of 2 := 1];
                   a[0 of 2 := 2];
                   a[0 of 2 := 3];
                    a" "" "(3, 1)";
    tgc "seq3" 20 "let f = (lambda(n): (1, 1, 1)) in
                   f(1);
                   f(2);
                   f(3);" "" "(1, 1, 1)";
    tgc "selfsend" 25 "let f = (lambda(x): 1) in
                        f(f(f(f(f(1)))))" "" "";
    tgc "multiplesend" 20 "let f = (lambda(x): x) in
                            let g = (lambda(x): f(x)) in
                            f(g(f(1)))"  "" "1";

    tgc "recursive" 10 "let f = (lambda(x): if x == 0: x else: f(x - 1)) in f(2)" "" "0";
    tgc "morerecursion" 20 "let f = (lambda(x): if x == 0: x else: f(x - 1)) in f(4)" "" "0";

    tgc "seq_rec" 10 "let f = (lambda(x): if x == 0: x else: f(x - 1)) 
                        in 
                        f(2);
                        f(0);
                        f(1);
                        f(2);
                        f(1)" "" "0";

    tgc "more_seq_rec" 20 "let f = (lambda(x): if x == 0: x else: f(x - 1)) 
                        in 
                        f(1);
                        f(2);
                        f(4);
                        f(4);
                        f(1)" "" "0";

    tgcerr "oomnow" 20 "let f = (lambda(x): if x == 0: x else: f(x - 1)) 
                        in f(100)" "" "Out of memory";

    tgcerr "oomagain" 10 "let f = (lambda(x, y, z): 0) in f(1, 2, 3)" "" "Out of memory";

   
]


let failing_tests = [

     tgcerr "gctest7" 3 "(1, (3, (4, 5)))" "" "Out of memory";

]

let suite =
"suite">:::
 pair_tests @ oom @ gc



let () =
  run_test_tt_main suite
;;

