structure Rational :> rational_sig =
struct
  datatype rational = Whole of int | Frac of int * int
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

  (* 约分化简 *)
  fun reduce(x, y) = 
      let
        val x' = if x < 0 andalso y < 0 then ~x else x
        val y' = if x < 0 andalso y < 0 then ~y else y
        val gcd_val = gcd(x', y')
      in
        if gcd_val <> abs y' then Frac(x' div gcd_val, y' div gcd_val)
        else Whole(x' div y')
      end

  fun make_whole x = 
      Whole x

  fun make_frac(x, y) = 
      if y = 0 then raise invalid_frac
      else if x = 0 then Whole 0
      else reduce(x, y)    

  fun add(x, y) = 
      case (x, y) of
        (Whole(i), Whole(j)) => Whole (i + j)
      | (Frac(ui, di), Frac(uj, dj)) => reduce(ui * dj + uj * di, di * dj)
      | (Whole(i), Frac(upper, down)) => reduce(i * down + upper, down)
      | (Frac(upper, down), Whole(i)) => reduce(i * down + upper, down)
      
  fun to_string x = 
      case x of
        Whole(i) => Int.toString i
      | Frac(upper, down) => 
          let
            val num = reduce(upper, down)
          in
            case num of
               Whole i => Int.toString i
             | Frac(upper, down) => Int.toString upper ^ "/" ^ Int.toString down
          end
         
  
  fun test_case (z) = 
      let
        val x = to_string(add(Frac(3, ~7), Frac(4, 5)))
        val y = to_string(add(make_frac(1, ~2), make_frac(~7, 2)))
      in
        if z then x else y
      end
end

signature rational_sig =
sig
  type rational(*abstract*)
  val make_whole:int -> rational
  val make_frac:int * int -> rational
  val add:rational * rational -> rational
  val to_string:rational -> string
  val test_case:bool -> string
end