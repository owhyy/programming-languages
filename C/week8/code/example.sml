(* functional approach for a arithmetic programming language: *)
(* tldr: program has more FUNCTIONS THAT OPERATE ON THE DATA: use FUNCTIONAL *)

exception BadResult of string

(* we'll have one datatype for expressions, which will have one constructor *)

datatype exp =
    Int of int
  | Negate of exp
  | Add of exp * exp

(* we then have a evaluating function, that will take an expression and evaluate
* it *)

fun eval e =
  case e of
       Int _ => e
     | Negate e1 => (case eval e1 of
                          Int i => Int (~i)
                        | _ => raise BadResult "non-int in negation")
     | Add(e1, e2) => (case (eval e1), (eval e2) of
                            Int i andalso Int j => Int (i + j)
                          | _ => raise BadResult "non-ints in addition")

(* finally, we have functions that will use the expressions *)

fun toString e =
  case e of
       Int i => Int.toString i
     | Negate e1 => "-(" ^ (toString e1) ^ ")"
     | Add(e1, e2) => "(" ^ (toString e1) ^ " + " ^ (toString e2) ^ ")"

fun hasZero e =
  case e of
       Int i => i = 0
     | Negate e1 => hasZero e1
     | Add(e1, e2) => (hasZero e1) orelse (hasZero e2)


(* adding a new function is easy *)
fun noNegConstants e =
  case e of
       Int i => if i < 0 then Negate (Int(~i)) else e
     | Negate e1 => Negate(noNegConstants e1)
     | Add(e1, e2) => Add(noNegConstants e1, noNegConstants e2)

(* however adding a new expression is harder, because we will need to go and add
* a new case for every one of our case expressions *)

(* supposing we add some new types: String and Rational to our language, this
* will ask for some new rules for addition; implementing them is quite
* straightforward *)

fun add_values (v1, v2)
  case (v1, v2) of
       (Int i, Int j) => Int (i + j)
     | (Int i, String s) => String(Int.toString i ^ s)
     | (Int i, Rational(j, k)) => Rational(i*k+j, k)
     | (String s, Int i) => String(s ^ Int.toString i)
     | (String s1, String s2) => String(s1 ^ s2)
     | (String s, Rational(i, j)) => String(s ^ Int.toString i ^ "/" ^ Int.toString j)
     | (Rational _, Int _) => add_values(v2, v1)
     | (Rational(i, j), String s) => String(Int.toString i ^ "/" ^ Int.toString j ^ s)
     | (Rational(a, b), Rational(c, d)) => Rational(a*d+b*c, b*d)
     | _ => raise BadResult "non-values passed to add_values"

