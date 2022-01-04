(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
string), then you avoid several of the functions in problem 1 having
polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

  (* put your solutions for problem 1 here *)

fun all_except_option(x, los) = 
  let fun all_except(x, los)=
  case los of
       [] => []
     | s::los' => if same_string(x, s) then all_except(x, los')
                  else s :: all_except(x, los')
    val new_los = all_except(x, los)
    in 
  if los = new_los then NONE else SOME(new_los)
  end

fun get_substitutions1(substitutions, x)=
  case substitutions of
      [] => []
    | los :: substitutions' => case (all_except_option(x, los)) of
                                SOME i => i@get_substitutions1(substitutions', x)
                              | NONE => get_substitutions1(substitutions', x)

fun get_substitutions2(substitutions, x)=
  let fun aux(substitutions, x, current)=
    case substitutions of
      [] => current
    | los :: substitutions' => case (all_except_option(x, los)) of
                                SOME i => aux(substitutions', x,
                                current@i)
                              | NONE => aux(substitutions', x,
                                current)
  in
    aux(substitutions, x, [])
  end

fun similar_names(substitutions, {first=f, middle=m, last=l})=
  let val found_subst = get_substitutions2(substitutions, f)
    fun append_first_rest(los, {first=a, middle=b, last=c})=
      case los of
           [] => []
         | x::los' => {first=x,middle=b,last=c}::append_first_rest(los', {first=f,middle=m,last=l})
  in
    append_first_rest(f::found_subst, {first=f, middle=m, last=l})
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c=
  case c of
       ((Clubs | Spades), rank) => Black
     | ((Diamonds | Hearts), rank)  => Red

fun card_value c=
  case c of
       (suit, Ace) => 11
     | (suit, Num i) => i 
     | (suit, (Jack | Queen | King)) => 10

fun remove_card (cs, c, e)= 
  case cs of
       [] => raise e
     | x::cs' => if x = c then cs' else x::remove_card(cs', c, e)

fun all_same_color cs =
  case cs of
       [] => true
     | _::[] => true
     | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

fun sum_cards(cs)=
  let fun tail_rec(loc, acc)=
    case loc of
         [] => acc
       | c::loc' => tail_rec(loc', acc + card_value(c))
  in
    tail_rec(cs, 0)
  end

fun score(cs, n)=
  let val sum = sum_cards(cs)
    val prelim = 
    if(sum > n) 
    then 3*(sum-n) 
    else n-sum
  in 
    if(all_same_color(cs)) then prelim div 2 else prelim
  end

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

(* fun officiate(card_list, move_list, goal)= *)
(*   let fun player(held_cards, moves_rem, curr_score)= *)
(*   case moves_rem of *)
(*        [] => score(held_cards, goal) *)
(*        move::moves_rem' => case move of *)
(*                       Discard c => player(remove_card(c), moves_rem', score(held_cards, goal)) *)
(*                       Draw andalso hc = [] => score(held_cards, goal) *)
(*                       Draw andalso score(cl, goal) > goal => player() *)
(*                       Draw => officiate() *)
(*   in *)
(*     let fun draw_fun() *)
(*     case card_list of *)
(*          [] => score(held_cards, goal) *)
(*          x::cl => if(score(cl, goal) > goal) then draw_fun() else *)
(*            player(x::held_cards, moves_rem, curr_score) *)
(*     player([], ms, 0) *)
