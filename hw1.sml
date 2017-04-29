fun is_older(d1: int*int*int, d2: int*int*int) = 
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d1) > (#1 d2)
    then false
    else if (#2 d1) < (#2 d2)  
    then true
    else if (#2 d1) > (#2 d2)
    then false 
    else (#3 d1) < (#3 d2)           
         
                    
fun number_in_month(ds: (int*int*int) list, month: int) = 
    let fun nim_acc(ds: (int*int*int) list, month: int, acc: int) =
        if null ds
        then acc
        else if (#2 (hd ds)) = month
        then nim_acc(tl ds, month, acc+1)
        else nim_acc(tl ds, month, acc)
    in
        nim_acc(ds, month, 0)
    end 
    
fun number_in_months(ds: (int*int*int) list, months: int list) =
    let fun nim_acc(ds: (int*int*int) list, months: int list, acc: int) =
        if null months
        then acc
        else let val res = number_in_month(ds, hd months)
            in 
                nim_acc(ds, tl months, acc+res)
            end
    in 
        nim_acc(ds, months, 0)
    end
     
                         
fun dates_in_month(ds: (int*int*int) list, month: int) = 
    let fun dim_acc(ds: (int*int*int) list, month: int, acc: (int*int*int) list) =
        if null ds
        then acc
        else if (#2 (hd ds)) = month
        then dim_acc(tl ds, month, acc @ [(hd ds)])
        else dim_acc(tl ds, month, acc)
    in
        dim_acc(ds, month, [])
    end
    
fun dates_in_months(ds: (int*int*int) list, months: int list) =
    let fun dim_acc(ds: (int*int*int) list, months: int list, acc: (int*int*int) list) =
        if null months
        then acc
        else let val res = dates_in_month(ds, hd months)
            in 
                dim_acc(ds, tl months, acc @ res)
            end
    in 
        dim_acc(ds, months, [])
    end
    
fun get_nth(xs: string list, pos: int) = 
    let fun gn_acc(xs: string list, pos: int, acc: int) = 
        if acc = pos then (hd xs)
        else gn_acc((tl xs), pos, acc+1)
    in
        gn_acc(xs, pos, 1)
    end
    
fun date_to_string(date: int*int*int) = 
    let 
        val months = ["January", "February", "March","April", "May", "June", "July", "August", "September", "October", "November", "Decemer"]
        val year = Int.toString((#1 date))
        val month = get_nth(months,(#2 date))
        val day = Int.toString((#3 date))
    in
        month ^ " " ^ day ^ ", " ^ year
    end
    
fun number_before_reaching_sum(sum: int, xs: int list) = 
    let fun nbrs_acc(pos: int, xs: int list, acc: int, acc2:int) = 
        if acc+(hd xs) >= sum then acc2
        else nbrs_acc(sum, tl xs, acc+(hd xs), acc2+1)
    in
        nbrs_acc(sum, xs, 0, 0)
    end
    
fun what_month(days: int) = 
    let 
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(days, months) +1
    end
    
fun month_range(d1: int, d2: int) = 
    let 
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val m1 = what_month(d1)
        val m2 = what_month(d2)
        fun mr(d1, d2, acc) = 
            if d1>d2 then acc
            else mr(d1+1, d2, acc @ [what_month(d1)])
    in
        mr(d1, d2, [])
    end
    
fun oldest(dates: (int * int * int) list) =
        let fun o_acc(dates, acc) = 
            if null dates then acc
            else if(is_older(hd dates, acc)) then o_acc(tl dates, hd dates)
            else o_acc(tl dates, acc)
        in
            if null dates then NONE
            else SOME (o_acc(tl dates, hd dates))
        end
