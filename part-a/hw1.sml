fun is_older(date1: (int * int * int), date2: (int * int * int)) =
    let
        val (y1, m1, d1) = date1;
        val (y2, m2, d2) = date2;
    in
        if (y1 < y2)
           orelse (y1 = y2 andalso m1 < m2)
           orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2) then
            true
        else
            false
    end

fun number_in_month(dates: (int * int * int) list, month: int) =
    if null dates then
        0
    else
        let val rest = number_in_month((tl dates), month)
        in if (#2 (hd dates)) = month then
               1 + rest
           else
               rest
        end

(* Assume that there is no repeated number in months *)
fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null dates orelse null months then
        0
    else
        number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null dates then
        []
    else
        let val rest = dates_in_month((tl dates), month)
        in if (#2 (hd dates)) = month then
               (hd dates) :: rest
           else
               rest
        end

fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null dates orelse null months then
        []
    else
        dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

fun get_nth(strings: string list, n: int) =
    if n = 1 then
        hd strings
    else
        get_nth((tl strings), n - 1)

fun date_to_string(date: (int * int * int)) =
    let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_names, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) =
    let fun helper(current_sum: int, numbers: int list, n: int) =
            if (hd numbers) + current_sum < sum then
                helper(current_sum + (hd numbers), (tl numbers), n + 1)
            else
                n
    in helper(0, numbers, 0)
    end

fun what_month(day_of_year: int) =
    let val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day_of_year, days_of_months) + 1
    end

fun month_range(day1: int, day2: int) =
    if day1 > day2 then
        []
    else
        what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
    if null dates then
        NONE
    else
        let fun helper(current_oldest: (int * int * int), dates: (int * int * int) list) =
                if null dates then
                    SOME current_oldest
                else
                    if is_older((hd dates), current_oldest)
                    then helper((hd dates), (tl dates))
                    else helper(current_oldest, (tl dates))
        in helper((hd dates), (tl dates))
        end

fun sorted []      = []
  | sorted (x::xs) =
    let val left = List.filter (fn (e) => e <= x) xs;
        val right = List.filter (fn (e) => e > x) xs;
    in
        sorted(left) @ [x] @ sorted(right)
    end

fun deduplicate []      = []
  | deduplicate (x::xs) =
    let fun helper(xs, last) =
            case xs of
                [] => []
              | x::xs' => if x = last then
                              helper(xs', last)
                          else
                              x::helper(xs', x)
    in
        x::helper(xs, x)
    end

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
    let val no_dup = deduplicate(sorted(months))
    in
        number_in_months(dates, months)
    end

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
    let val no_dup = deduplicate(sorted(months))
    in
        dates_in_months(dates, months)
    end

fun reasonable_date(date: int * int * int) =
    let val (year, month, day) = date;
        fun is_valid_day(year, month, day) =
            let fun is_leap_year(year) =
                    ((year mod 400) = 0)
                    orelse (((year mod 100) <> 0)
                            andalso ((year mod 4) = 0))
                val days_of_months =
                    if is_leap_year(year) then
                        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    else
                        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                val days = List.nth(days_of_months, month - 1)
            in 1 <= day andalso day <= days
            end
    in
        year > 0
        andalso (1 <= month andalso month <= 12)
        andalso is_valid_day(year, month, day)
    end
