fun search(xs:int list, elm:int) = 
    if null xs then false
    else if hd xs = elm then true
    else search(tl xs, elm)

fun unique(xs:int list, unique_list:int list) =                 
    if null xs then unique_list
    else if search(unique_list, hd xs) = false then unique(tl xs, unique_list::hd xs)
    else unique(tl xs, unique_list)            

fun number_in_months(dates:(int*int*int)list, months:int list) =     
    var unique_months = unique(months, [])
    if null unique_months then 0
    else
        number_in_month(dates, hd unique_months) + number_in_months(dates, tl unique_months)

fun number_in_month(dates:(int*int*int)list, month) = 
    if null dates then 0
    else 
        if #2 hd dates = month then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)
