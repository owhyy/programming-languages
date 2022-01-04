
fun accumulate(xs: int list)=
  if null xs then 0 
  else (hd xs) + accumulate(tl xs);
fun alternate_list(numbers: int list)=
  if null numbers
  then []
  else let fun alternate_sign(count:int, xs: int list)=
  if count > length numbers
  then []
  else if count mod 2 = 0
  then ~(hd xs)::alternate_sign(count + 1, tl xs)
  else hd xs::alternate_sign(count + 1, tl xs)
       in
         (alternate_sign(1, numbers))
       end;
fun alternate(numbers: int list)=
  accumulate(alternate_list(numbers));

fun min_max(xs: int list)=
  let fun find_min(curr_min: int, curr_xs: int list)=
  if null curr_xs 
  then curr_min
  else
    if curr_min > hd curr_xs
    then find_min(hd curr_xs, tl curr_xs)
    else find_min(curr_min, tl curr_xs)

    fun find_max(curr_max: int, curr_xs: int list)=
      if null curr_xs 
      then curr_max
      else
        if curr_max < hd curr_xs
        then find_max(hd curr_xs, tl curr_xs)
        else find_max(curr_max, tl curr_xs)
       in
         (find_min(hd xs, xs), find_max(hd xs, xs))
       end;

(* ??? *)
(* fun cumsum(xs: int list)= *)
(*   let fun sum(count) *)

fun greeting(name:string option)=
  if isSome name 
  then "Hello there, " ^ valOf name ^ "!"
  else "Hello there, you!";
