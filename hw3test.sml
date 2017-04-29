(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test1_1 = only_capitals ["A","B","cafa"] = ["A","B"]

val test1_2 = only_capitals ["A","dfafa","B","C"] = ["A","B","C"]

val test1_3 = only_capitals [] = []

val test1_4 = only_capitals ["dfa", "kjo"] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test2_1 = longest_string1 [] = ""

val test2_2 = longest_string1 ["Ac","bc","C"] = "Ac"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test3_1 = longest_string2 [] = ""

val test3_2 = longest_string2 ["Ac","bc","C"] = "bc"

val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A";

val test6 = rev_string "abc" = "cba";

val test6_1 = rev_string "" = "";

val test6_2 = rev_string "gfabc" = "cbafg";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8_1 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test8_3 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []

val test8_4 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1

val test9a_1 = count_wildcards (Variable("a")) = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b_1 = count_wild_and_variable_lengths Wildcard = 1

val test9b_2 = count_wild_and_variable_lengths (TupleP([Variable("a"), Variable("arca"), Variable("bacca")])) = 10

val test9b_3 = count_wild_and_variable_lengths (TupleP([Variable("a"), Wildcard, Variable("bacca")])) = 7

val test9b_4 = count_wild_and_variable_lengths (TupleP([Variable("a"), Wildcard, Variable("bacca"), Wildcard])) = 8

val test9c = count_some_var ("x", Variable("x")) = 1;

val test9c_1 = count_some_var ("x", TupleP([Variable("a"), Wildcard, Variable("bacca"), Wildcard])) = 0;

val test9c_2 = count_some_var ("bacca", TupleP([Variable("a"), Wildcard, Variable("bacca"), Wildcard])) = 1;

val test9c_3 = count_some_var ("a", TupleP([Variable("a"), Wildcard, Variable("a"), Variable("bacca"), Variable("a"),Wildcard])) = 3;

val test10 = check_pat (Variable("x")) = true

val test10_1 = check_pat (TupleP([Variable("a"), Wildcard, Variable("a"), Variable("bacca"), Variable("a"),Wildcard])) = false;

val test10_2 = check_pat (TupleP([Variable("a"), Wildcard, Variable("v"), Variable("bacca"), Variable("a"),Wildcard])) = false;

val test10_3 = check_pat (TupleP([Variable("t"), Wildcard, Variable("v"), Variable("bacca"), Variable("a"),Wildcard])) = true;

val test10_4 = check_pat (TupleP([Variable("h"), Wildcard, Variable("h"), Variable("bacca"), Variable("a"),Wildcard])) = false;

val test10_5 = check_pat (Wildcard) = true;

val test11 = match (Const(1), UnitP) = NONE

val test11_1 = match (Const(1), ConstP(1)) = SOME []

val test11_2 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]) = SOME []

val test_11_2 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val test12_1 = first_match Unit [Wildcard] = SOME []

val test12_2 = first_match (Const(1)) [Wildcard] = SOME[]

val test12_3 = first_match (Const(1)) [UnitP] = NONE


