(* #1 is year, #2 is month, #3 is day *)
fun is_older(d1:int*int*int, d2:int*int*int)=
  if (#1 d1 < #1 d2) orelse
  (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse 
  (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2) 
  then true
  else false;

fun number_in_month(dates:(int*int*int) list, month:int)=
  if null dates 
  then 0
  else 
    if (#2 (hd dates)) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month);

fun number_in_months(dates:(int*int*int) list, months: int list)=
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month(dates:(int*int*int) list, month: int)=
  if null dates
  then []
  else
    if (#2 (hd dates)) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month);

fun dates_in_months(dates:(int*int*int) list, months: int list)=
  if null months
  then []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, nth: int)=
  let fun iter(i: int, s: string list)= 
  if i = nth
  then hd s
  else iter(i+1, tl s)
  in
    iter(1, strings)
  end;

fun date_to_string(date: int*int*int)=
  let val months=["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date)  ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end;

fun number_before_reaching_sum(sum: int, values:int list)=
  let fun adder(value:int, count:int, values_remaining: int list)=
  if value >= sum
  then count - 1
  else adder(hd values_remaining + value, count + 1, tl values_remaining)
  in
    adder(0, 0, values)
  end;

fun what_month(day: int)=
  let val months=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
    number_before_reaching_sum(day, months) + 1
  end;

fun month_range(day1: int, day2: int)=
  if day1>day2 
  then []
  else what_month(day1) :: month_range(day1+1, day2);

fun oldest(dates:(int*int*int) list)=
  if null dates 
  then NONE
  else 
    let fun find_oldest(current_oldest: int*int*int, rest:(int*int*int) list)=
    if null rest
    then SOME current_oldest
    else if is_older(hd rest, current_oldest) 
    then find_oldest(hd rest, tl rest)
    else find_oldest(current_oldest, tl rest)
  in
    find_oldest(hd dates, dates)
  end;

fun number_in_months_challenge(dates:(int*int*int) list, months: int list)=
  number_in_months(dates, remove_dupls(months))

fun dates_in_months_challenge(dates:(int*int*int) list, months: int list)=
  dates_in_months(dates, remove_dupls(months))

fun member(x: int, xs: int list)=
  if null xs then false
  else if x=hd xs then true
  else member(x, tl xs);

fun remove_dupls(xs: int list)=
  if null xs
  then []
  else if member((hd xs), (tl xs))
  then remove_dupls(tl xs)
  else (hd xs)::remove_dupls(tl xs);

fun reasonable_date(date:int*int*int)=
  if #1 date > 0 andalso (#2 date > 0 andalso #2 date < 13) andalso (#3 date > 0
  andalso #3 date < 31) 
       then if (#1 date mod 4 = 0) orelse (#1 date mod 100 = 0 andalso #1 date mod
       400 = 0)
  then true
  else false
  else false;

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12 =
  number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,2,3,3,3,4]) = 3

val test13 =
  dates_in_months_challenge
  ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,2,2,3,3,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test14 = 
  reasonable_date(0, 10, 3)=false

val test15 = 
  reasonable_date(2000, 15, 10)=false

val test16 = 
  reasonable_date(2000, 10, 50)=false

val test17 = 
  reasonable_date(2012, 2, 29)=true

val test18 = 
  reasonable_date(2013, 2, 29)=false
