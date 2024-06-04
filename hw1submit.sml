fun is_older (fd : int*int*int, fd2 : int*int*int) =
	if #1 fd < #1 fd2 then true
	else if #1 fd > #1 fd2 then false
	else if #2 fd < #2 fd2 then true
	else if #2 fd > #2 fd2 then false
        else #3 fd < #3 fd2

fun  number_in_month (dL : (int*int*int) list, mo : int) =
     if null dL
     then 0
     else
	 let
	     val m = #2 (hd dL)
	 in
	     if m = mo
	     then 1 + number_in_month (tl dL, mo)
	     else number_in_month (tl dL, mo)
	 end

fun number_in_months (dL : (int*int*int) list, mL : int list) =
    if null mL 
    then 0
    else
	let
	    val a = hd mL
	in
	    number_in_month(dL, a) + number_in_months(dL, tl mL)
	end
	    
fun dates_in_month (dL : (int*int*int) list, mo : int) =
    if null dL
    then []
    else
        let
            fun iterate_over_dL (dL : (int*int*int) list) =
                if null (tl dL)
                then
		    if #2 (hd dL) = mo
		    then [hd dL]
		    else []
                else
                    let
                        val rest = iterate_over_dL(tl dL)
                    in
                        if #2 (hd dL) = mo
                        then hd dL :: rest
                        else rest
                    end
        in
            iterate_over_dL dL
        end

fun dates_in_months (dL : (int*int*int) list, mL : int list) =
    if null mL 
    then []
    else
	let
	    val a = hd mL
	in
	    dates_in_month(dL, a) @ dates_in_months(dL, tl mL)
	end


fun get_nth (l : string list, n : int) =
    if null l
    then ""
    else if null (tl l) andalso n <> 1
    then ""
    else if n = 1
    then hd l
    else
	let
	   val x = tl l
	in
	    get_nth (x, n-1)
	end


fun date_to_string (d : int*int*int) =
    let
	val months = ["January","February","March","April","May","June","July","August","September",
		      "October","November","December"]
    in
	get_nth(months,#2 d) ^ " " ^ Int.toString(#3 d) ^ "," ^ " " ^ Int.toString(#1 d)
    end



fun number_before_reaching_sum (sum : int, lst : int list) =
    let
	fun helper(lst: int list, count : int, curr_sum : int) =
	    if null lst
	    then count
	    else if curr_sum + hd lst >= sum then count
	    else helper(tl lst, count + 1, curr_sum + hd lst)
    in
	helper (lst, 0, 0)
    end

fun what_month (day : int) =
    let
	val mo_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]		   
    in
	number_before_reaching_sum(day, mo_days) + 1
    end
	

fun month_range (day1 : int, day2 : int) =
	let
	    fun countdown (day1 : int, day2 : int) =
		if day1 > day2 
		then []
		else what_month(day1) :: countdown(day1 + 1, day2)
	in
	    countdown(day1,day2)
	end
	    

fun oldest( lst : (int*int*int) list) =
    if null lst
    then NONE
    else
	let
	    fun oldest_nonempty(lst : (int*int*int) list) =
		if null (tl lst)
		then hd lst
		else
		    let
			val tl_oldest = oldest_nonempty(tl lst)
		    in
			if is_older(hd lst, tl_oldest)
			then hd lst
			else tl_oldest
		    end
	in
	    SOME (oldest_nonempty lst)
	end
			

fun de_dup (mL : int list) =
    if null mL
    then []
    else if null (tl mL)
    then [hd mL]
    else
	let
	    val de_dup_tl = de_dup(tl mL)
	in
	    if hd mL <> hd de_dup_tl
	    then hd mL :: de_dup_tl
	    else de_dup_tl
	end

fun number_in_months_challenge (dL : (int*int*int) list, mL : int list) =
    if null mL 
    then 0
    else
	let
	    val unique_mL = de_dup mL
	    val a = hd unique_mL
	    val unique_tl = tl unique_mL
	in
	    number_in_month(dL, a) + number_in_months(dL, unique_tl)
	end


fun dates_in_months_challenge (dL : (int*int*int) list, mL : int list) =
    if null mL 
    then []
    else
	let
	    val unique_mL = de_dup mL
	    val a = hd unique_mL
	    val unique_tl = tl unique_mL
	in
	    dates_in_month(dL, a) @ dates_in_months(dL, unique_tl)
	end


	    
fun get_nth_int (l : int list, n : int) =
    if null l
    then 0
    else if null (tl l) andalso n <> 1
    then 0
    else if n = 1
    then hd l
    else
	let
	   val x = tl l
	in
	    get_nth_int (x, n-1)
	end


	    
fun reasonable_date (d : int*int*int) =
    if #1 d <= 0
    then false
    else if 0<= #2 d orelse #2 d > 12  
    then false
    else
	let
	    val mo_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val mo_days_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    fun check_leap(d : int*int*int) =
		if (#1 d mod 400) = 0
		then true
		else if (#1 d mod 4) = 0 andalso (#1 d mod 100) <> 0
		then true
		else false
	in
	    if not(check_leap d) andalso #3 d <= get_nth_int(mo_days, #2 d)
	    then true
	    else if check_leap d andalso #3 d <= get_nth_int(mo_days_leap, #2d)
	    then true
	    else false
	end
