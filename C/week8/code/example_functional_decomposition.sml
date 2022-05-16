(* implementanig binary methods for more than 1 type is pretty straightforward,
* altough not most beautiful for functional programming: *)

exception BadResult of string

datatype exp =
    Int of int
  | String of string
  | Rational of int * int
  | Negate of exp
  | Add of exp * exp

fun eval e =
  case e of
       Int _ => e
     | String _ => ...
     | Rational(j, k) => ...
     | Negate e1 => ...
     | Add(e1, e2) => add_values(e1, e2) (* method that behaves differently,
     depending on the types called *)

fun toString e =
  case e of
       Int i => Int.toString i
     | Negate e1 => "-(" ^ (toString e1) ^ ")"
     | Add(e1, e2) => "(" ^ (toString e1) ^ " + " ^ (toString e2) ^ ")"

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

