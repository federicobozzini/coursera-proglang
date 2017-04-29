(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

val test9 = what_month(70) = 3

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12 = is_older((2,2,3),(2,3,4)) = true

val test13 = is_older((3,2,3),(2,3,4)) = false

val test14 = is_older((2,3,3),(2,3,4)) = true

val test15 = is_older((2,3,4),(2,3,4)) = false

val test16 = is_older((2,4,4),(2,3,4)) = false

val test17 = is_older((2,3,4),(2,3,3)) = false

val test18 = number_in_month([],2) = 0

val test19 = number_in_month([(2012,2,28),(2013,12,1)],1) = 0

val test20 = number_in_month([(2012,2,28),(2013,12,1),(2013,12,1),(2013,12,1)],12) = 3

val test21 = number_in_months([],[2,3,4]) = 0

val test22 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0

val test23 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = 1

val test24 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(2011,4,28),(2011,4,28)],[2,4]) = 4

val test25 = dates_in_month([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]

val test26 = number_before_reaching_sum(2, [1,2,3,4,5]) = 1

val test27 = number_before_reaching_sum(5, [3,1,2]) = 2

val test28 = number_before_reaching_sum(6, [4,1,1,1]) = 2

val test29 = what_month(1) = 1

val test30 = what_month(41) = 2

val test31 = what_month(350) = 12

val test32 = what_month(180) = 6

val test33 = number_before_reaching_sum(1, [2]) = 0

val test34 = month_range(39, 34) = []

val test35 = month_range(58, 63) = [2, 2, 3, 3, 3, 3]

val test36 = oldest([]) = NONE

val test37 = oldest([(2012,1,23),(2011,3,31),(2011,3,7)]) = SOME (2011,3,7)

