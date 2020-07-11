fun is_older(date1: (int * int * int), date2: (int * int * int)) =
    let
        val (y1, m1, d1) = date1;
        val (y2, m2, d2) = date2;
    in
        if y1 < y2
        then true
        else if y1 > y2
             then false
             else if m1 < m2
                  then true
                  else if m1 > m2
                       then false
                       else if d1 < d2
                            then true
                            else false
    end

fun number_in_month(dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else
        let val rest = number_in_month((tl dates), month)
        in if (#2 (hd dates)) = month
           then 1 + rest
           else rest
        end

(* Assume that there is no repeated number in months *)
fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null dates orelse null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))


fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else
        let val rest = dates_in_month((tl dates), month)
        in if (#2 (hd dates)) = month
           then (hd dates) :: rest
           else rest
        end

fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

fun get_nth(strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth((tl strings), n - 1)

fun date_to_string(date: (int * int * int)) =
    let val date_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_names, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) =
    let fun __helper(current_sum: int, numbers: int list, n: int) =
            if (hd numbers) + current_sum < sum
            then __helper(current_sum + (hd numbers), (tl numbers), n + 1)
            else n
    in __helper(0, numbers, 0)
    end

fun what_month(day_of_year: int) =
    let val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day_of_year, days_of_months)
    end

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let fun __helper(current_oldest: (int * int * int), dates: (int * int * int) list) =
                if null dates
                then SOME current_oldest
                else
                    if is_older((hd dates), current_oldest)
                    then __helper((hd dates), (tl dates))
                    else __helper(current_oldest, (tl dates))
        in __helper((hd dates), (tl dates))
        end

fun 
