datatype rational = 
        Whole of int
    |   Frac of int * int

exception invalid_frac

fun gcd(x, y) = 
    let
      val max = if x > y then x else y
      val min = if x > y then y else x      
      fun gcd_aux(x, y) =
        if x > y then gcd_aux(x - y, y)
        else if x < y then gcd_aux(y - x, x)
        else x
    in
      if min = 0 orelse max = 0 then 1
      else if min > 0 then gcd_aux(max - min, min)
      else if max < 0 then gcd_aux(abs max, abs min)
      else gcd_aux(~min, max)
    end 

fun reduce x = 
    case x of
       Whole i => Whole i
     | Frac(upper, down) => 
        if down = 0 then raise invalid_frac
        else if upper = 0 then Whole 0
        else
            let
                val upper' = if upper < 0 andalso down < 0 then ~upper else upper
                val down' = if upper < 0 andalso down < 0 then ~down else down
                val gcd_val = gcd(upper', down')
            in
                if gcd_val <> down' then Frac(upper' div gcd_val, down' div gcd_val)
                else Whole(upper' div gcd_val)
            end

fun add(x, y) = 
    case (x, y) of
       (Whole(i), Whole(j)) => Whole (i + j)
     | (Frac(ui, di), Frac(uj, dj)) => reduce(Frac(ui * dj + uj * di, di * dj))
     | (Whole(i), Frac(upper, down)) => reduce(Frac(i * down + upper, down))
     | (Frac(upper, down), Whole(i)) => reduce(Frac(i * down + upper, down))
    

fun to_string x = 
    case x of
       Whole(i) => Int.toString i
     | Frac(upper, down) => Int.toString upper ^ "/" ^ Int.toString down


val x = to_string(add(Frac(3, ~7), Frac(4, 5)))