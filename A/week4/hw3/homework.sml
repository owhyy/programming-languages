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
(* r is the function that applies g f1 and f2 *)
(* if p is a wildcard, f1 will be applied on everything??? *)
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

(* for the challenge problem only *)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(* you can put all your code here *)

fun only_capitals los =
    List.filter (fn x => Char.isUpper (String.sub (x, 0))) los;

fun longest_string1 los =
    List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" los;

fun longest_string2 los =
    List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" los;

fun longest_string_helper f =
    List.foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" ;

val longest_string3 =
    longest_string_helper (fn (x, y) => x > y);

val longest_string4 =
    longest_string_helper (fn (x, y) => x >= y);

val longest_capitalized =
    longest_string1 o only_capitals;

fun rev_string s =
    (implode o rev o explode) s;

fun first_answer f l =
    case l of
         [] => raise NoAnswer
       | x::xs => case f x of
                       SOME e => e
                     | NONE => first_answer f xs

fun all_answers f l =
    let fun aux l acc =
            case l of
                 [] => SOME acc
               | x::xs => case f x of
                               SOME e => aux xs (e @ acc)
                             | NONE => NONE
    in
    aux l []
    end;

val count_wildcards =
    g (fn _ => 1) (fn _ => 0);

val count_wild_and_variable_lengths =
    g (fn _ => 1) String.size;

fun count_some_var (s, p) =
    g (fn _ => 0) (fn x => if x = s then 1 else 0) p;

(* true if all the variables are distinct *)
(* 1. patern -> list of all strings it uses for variables *)
(* 2. list of strings -> true if has repeats, false otherwise *)

fun check_pat p =
    let fun lo_variable_names (p, acc) =
            case p of
                 Variable e => e::acc
               | TupleP e => List.foldl lo_variable_names acc e
               | ConstructorP(_, p) => lo_variable_names (p, acc)
               | _ => acc
        fun all_distinct los =
            case los of
                 [] => true
               | a::b => (not (List.exists (fn x => x = a) b)) andalso (all_distinct b)
    in
    all_distinct (lo_variable_names (p, []))
    end;

fun match (v, p) =
    case (v, p) of
         (_, Wildcard) => SOME []
       | (v, Variable s) => SOME [(s, v)]
       | (Unit, UnitP) => SOME []
       | (Const v, ConstP n) => if v=n then SOME [] else NONE
       | (Tuple vs, TupleP  ps)  => if List.length vs = List.length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
       | (Constructor(s1, v), (ConstructorP(s2, p))) => if s1 = s2 then match (v, p) else NONE
       | _ => NONE


fun first_match v lop =
    SOME (first_answer (fn p => match(v, p)) lop)
    handle NoAnswer => NONE

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","bicnananaiski","C"] = "bicnananaiski"

val test2_tie = longest_string1 ["ana","are","mer"] = "ana"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test3_tie = longest_string2 ["A","bc","C","cd"] = "cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
