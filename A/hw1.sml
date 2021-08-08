(* Problem 1 *)

fun is_older(x: int * int * int, y: int * int * int) = 
    if #1 x < #1 y
    then true
    else if (#1 x = #1 y) andalso (#2 x < #2 y)
        then true
        else if (#1 x = #1 y) andalso (#2 x = #2 y) andalso (#3 x < #3 y)
            then true
            else false



(* Problem 2 *)

fun number_in_month(x: (int * int * int) list, y: int) =
    if null x
    then 0
    else if #2 (hd x) = y
        then 1 + number_in_month(tl x, y)
        else number_in_month(tl x, y)



(* Problem 3 *)

fun number_in_months(x: (int * int * int) list, y: int list) =
    if null y
    then 0
    else number_in_month(x, hd y) + number_in_months(x, tl y)



(* Problem 4 *)

fun dates_in_month(x: (int * int * int) list, y: int) = 
    if null x
    then []
    else if #2 (hd x) = y
        then (hd x)::dates_in_month(tl x, y)
        else dates_in_month(tl x, y)



(* Problem 5 *)

fun dates_in_months(x: (int * int * int) list, y: int list) =
    if null y
    then []
    else dates_in_month(x, hd y) @ dates_in_months(x, tl y)



(* Problem 6 *)

fun get_nth(s: string list, n: int) =
    if n = 1
    then hd s
    else get_nth(tl s, n-1)



(* Problem 7 *)

fun date_to_string(d: int * int * int) = 
    let val months = ["January", "February", "March", "April", 
                    "May", "June", "July", "August", "September", 
                    "October", "November", "December"]
    in  
    get_nth(months,#2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
    end



(* Problem 8 *)

fun number_before_reaching_sum(sum: int, x: int list) = 
    if sum <= hd x
    then 0
    else 1 + number_before_reaching_sum(sum - (hd x), tl x)



(* Problem 9 *)

fun what_month(x: int) = 
    1 + number_before_reaching_sum(x,[31,28,31,30,31,30,31,31,30,31,30,31])



(* Problem 10 *)

fun month_range(d1: int, d2: int) = 
    if d1 > d2
    then []
    else what_month(d1)::month_range(d1+1,d2)



(* Problem 11 *)

fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else let fun oldest_nonempty(dates: (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else let val current_oldest = oldest_nonempty(tl dates)
                    in if is_older(hd dates, current_oldest)
                        then hd dates
                        else current_oldest
                    end
        in SOME (oldest_nonempty(dates))
        end



(* Problem 12 *)

fun is_in_list(x: int list, y: int) = (* Checks if y is in list x, returns boolean *)
    if null x
    then false
    else if (hd x) = y
        then true
        else is_in_list(tl x, y)

fun remove_duplicates(x: int list) = (* Returns list without repeated elements *)
    if null x
    then []
    else if is_in_list(tl x, hd x)
    then remove_duplicates(tl x)
    else (hd x)::remove_duplicates(tl x)

fun number_in_months_challenge(x: (int * int * int) list, y: int list) =
    number_in_months(x,remove_duplicates(y))

fun dates_in_months_challenge(x: (int * int * int) list, y: int list) =
    dates_in_months(x,remove_duplicates(y))



(* Problem 13 *)

fun days_per_month(year: int) = (* Returns list of days per month depending on leap years *)
    if (((year mod 4) = 0) andalso ((year mod 100) <> 0))
        orelse ((year mod 400) = 0)
    then [31,29,31,30,31,30,31,31,30,31,30,31]
    else [31,28,31,30,31,30,31,31,30,31,30,31]

fun get_nth_int(x: int list, n: int) = (* Same as in problem 6, but for int list *)
    if n = 1
    then hd x
    else get_nth_int(tl x, n-1)

fun reasonable_date(x: int * int * int) =
    if ((#1 x) < 1) orelse ((#2 x) < 1) orelse ((#2 x) > 12) orelse ((#3 x) < 1)
    then false
    else let val days_in_month = days_per_month(#1 x)
        in if (#3 x) > get_nth_int(days_in_month, #2 x)
            then false
            else true
        end