(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* fun only_capitals(xs)=filter((fn (v)=>(hd v).isUpper), xs); *)


(* val test1 = only_capitals ["A","B","C"] = ["A","B","C"]*)

(* val test2 = longest_string1 ["A","bc","C"] = "bc"*)

(* val test3 = longest_string2 ["A","bc","C"] = "bc"*)

(* val test4a = longest_string3 ["A","bc","C"] = "bc"*)

(* val test4b = longest_string4 ["A","B","C"] = "C"*)

(* val test5 = longest_capitalized ["A","bc","C"] = "A"*)

(* val test6 = rev_string "abc" = "cba"*)

(* val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4*)

(* val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE*)

(* val test9a = count_wildcards Wildcard = 1*)

(* val test9b = count_wild_and_variable_lengths (Variable("a")) = 1*)

(* val test9c = count_some_var ("x", Variable("x")) = 1*)

(* val test10 = check_pat (Variable("x")) = true*)

(* val test11 = match (Const(1), UnitP) = NONE*)

(* val test12 = first_match Unit [UnitP] = SOME []*)
