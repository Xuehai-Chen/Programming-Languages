fun is_older (date1 : int*int*int, date2 : int*int*int) = 
  if (#1 date1) < (#1 date2)
  then true
  else if ((#1 date1) = (#1 date2)) andalso ((#2 date1) < (#2 date2))
  then true
  else if  ((#1 date1) = (#1 date2)) andalso ((#2 date1) = (#2 date2)) andalso ((#3 date1) < (#3 date2))
  then true
  else false

fun in_month_list (month : int, months : int list) = 
  if null months
  then false
  else if month = hd months
  then true
  else in_month_list (month, tl months)

fun how_many_dates (month : int) =
  let val months_with_thirty_days= [4,6,9,11]
  in
    if month = 2 
    then 28 
    else if in_month_list(month, months_with_thirty_days)
    then 30
    else 31 
  end

fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates orelse month < 0 orelse month >12
  then 0
  else
    let val dates_of_month = how_many_dates month 
    in
        if month = (#2 (hd dates)) andalso dates_of_month >=(#3 (hd dates))
        then 1+number_in_month(tl dates,month)
        else number_in_month(tl dates,month)
    end

fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then 0
  else  number_in_month (dates, hd months) + number_in_months (dates, tl months) 

fun dates_in_month (dates : (int*int*int) list, month : int) =
  if null dates orelse month < 0 orelse month > 12
  then [] 
  else
    let val dates_of_month = how_many_dates month 
    in
        if month = (#2 (hd dates)) andalso dates_of_month >=(#3 (hd dates))
        then (hd dates)::(dates_in_month(tl dates,month))
        else dates_in_month(tl dates,month)
    end

fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else (dates_in_month(dates, hd months)) @ (dates_in_months(dates, tl months)) 
  
fun get_nth ( string_list : string list, number : int) =
  if null string_list
  then "" 
  else if number = 1
  then hd string_list
  else get_nth( tl string_list, number - 1)

fun date_to_string ( date : int*int*int ) =
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
     (get_nth( months, (#2 date))) ^" "^ Int.toString(#3 date) ^", "^Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, numbers : int list) =
  if sum <= 0
  then ~1
  else 1+ number_before_reaching_sum ( sum - (hd numbers), tl numbers)

fun what_month ( day : int)=
let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
in
  1+number_before_reaching_sum (day,months)
end

fun month_range (dayone : int, daytwo : int) =
  if dayone > daytwo
  then []
  else
    what_month (dayone)::month_range( dayone+1 , daytwo )

fun oldest_helper (days :(int*int*int) list, oldest : (int*int*int) ) = 
  if null days
  then oldest
  else if is_older(hd days, oldest)
  then
    let val newoldest = (hd days)
    in
        oldest_helper(tl days, newoldest)
    end
  else
    oldest_helper(tl days, oldest)

fun oldest (days : (int*int*int) list) =
  if null days
  then NONE
  else SOME(oldest_helper(tl days, hd days))
