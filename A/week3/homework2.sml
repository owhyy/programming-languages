(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
string), then you avoid several of the functions in problem 1 having
polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

  (* put your solutions for problem 1 here *)

  (* you may assume that Num is always used with values 2, 3, ..., 10
  though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun all_except_option(s, sl) = 
  let fun get_except(ql)=
  case ql of
       [] => []
     | x::xs' => if same_string(s, x) then get_except(xs') else x::get_except(xs')
    val result = get_except(sl) in
      if length result < length sl
      then SOME result
      else NONE
  end;

fun get_substitutions1(loxs, s) = 
  case loxs of
       [] => []
     | sl::sll' => let val result = all_except_option(s, sl) in
       case result of
            SOME x => x @ get_substitutions1(sll', s)
          | NONE => get_substitutions1(sll', s)
  end;

fun get_substitutions2(loxs, s) =
  let fun aux(loxs_aux, tail)=
  case loxs_aux of
       [] => tail
     | xl::xll' => let val result = all_except_option(s, xl) in
       case result of
            SOME x => aux(xll', x @ tail)
          | NONE => aux(xll', tail)
                   end
                   in
                     aux(loxs,[])
                   end;

fun similar_names(loxs, {first=f,middle=m,last=l })=
  let fun aux(substitutions,result)=
  case substitutions of
       [] => {first=f,middle=m,last=l}::result
     | x::xs => aux(xs, {first=x, middle=m, last=l}::result) in
       aux(get_substitutions2(loxs, f), [])
  end;

fun card_color(c) = 
  case c of
       (Clubs, _) => Black
     | (Spades, _) => Black
     |  _ => Red

fun card_value(c) = 
  case c of
       (_, Ace) => 11
     | (_, Num i) => i
     | _ => 10

fun remove_card(cs, c, e)=
  case cs of
       [] => raise e
     | x::cs' => if c = x then cs' else remove_card(cs', c, e);

fun all_same_color(cs)=
  case cs of
       [] => true
     | _::[] => true
     | head::(neck::rest) => (card_color(head) = card_color(neck) andalso
     all_same_color (neck::rest));

fun sum_cards(cs) = 
  let fun sum(cs', result)=
    case cs' of
         [] => result
       | c::cs'' => sum(cs'', card_value(c) + result)
  in
    sum(cs, 0)
  end;

fun score(cs, goal)=
  let val sum=sum_cards(cs)
    val prel_score = if sum > goal
                 then (3 * (sum - goal))
                 else (goal - sum)
  in
    if(all_same_color(cs)) then prel_score div 2 else prel_score
  end;

fun officiate(cs, ml, goal)=
  let fun helper(cc,hc, hm)=
  case hm of
       [] => score(hc, goal)
     | m::moves => case m of
                        Discard c => helper(cc, remove_card(hc, c, IllegalMove), moves)
                      | Draw => case cc of 
                                     [] => score(hc, goal)
                                   | c'::cc' => if score(c'::hc, goal) > goal then
                                       score(c'::hc, goal) else
                                         helper(cc', c'::hc, moves)
  in
    helper(cs, [], ml)
  end;

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option ("string", ["string", "something else"]) = SOME
["something else"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 =
  get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
  "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"];

val test3 = get_substitutions2 ([["foo", "bukowski"],["there"]], "foo") = ["bukowski"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
  [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
    {first="F", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}];

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

(* YEAH BABY *)
