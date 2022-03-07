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

fun only_capitals los =
    List.filter (fn x => Char.isUpper (String.sub (x, 0))) los;

fun longest_string1 los =
    List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" los;

fun longest_string2 los =
    List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" los;

fun longest_string_helper f =
    List.foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" ;

val longest_string3 =
 fn los => longest_string_helper (fn (x, y) => x>y) los;

val longest_string4 =
 fn los => longest_string_helper (fn (x, y) => x>=y) los;

val longest_capitalized =
 fn los => (longest_string1 o only_capitals) los;

fun rev_string s =
    (implode o rev o explode) s;

(* val first_answer = *)
(*   fn f => List.map (f, ) *)

(* val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4*)

fun map (f, xs) =
    case xs of
        [] => []
      | x::xs' =>  f(x)::map(f, xs');


(* fun count_wildcards p *)
(*                     case p of *)


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","bicnananaiski","C"] = "bicnananaiski"

val test2_tie = longest_string1 ["ana","are","mer"] = "ana"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test3_tie = longest_string2 ["A","bc","C","cd"] = "cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

(* val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4*)

(* val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE*)

(* val test9a = count_wildcards Wildcard = 1*)

(* val test9b = count_wild_and_variable_lengths (Variable("a")) = 1*)

(* val test9c = count_some_var ("x", Variable("x")) = 1*)

(* val test10 = check_pat (Variable("x")) = true*)

(* val test11 = match (Const(1), UnitP) = NONE*)

(* val test12 = first_match Unit [UnitP] = SOME []*)
