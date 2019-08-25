exception NoAnswer
datatype pattern = 
    Wildcard
  | Variable of string
  | UnitP
  | ConstP of int
  | TupleP of pattern list
  | ConstructorP of string * pattern
datatype valu = 
    Unit
  | Const of int
  | Tuple of valu list
  | Constructor of string * valu

fun map(xs, f) = 
    case xs of
       [] => []
     | x::xs' => f x :: map(xs', f)


fun filter(xs, f) =
    case xs of
       [] => []
     | x::xs' => 
        if f x then x :: filter(xs', f)
        else filter(xs', f)


fun foldl(f, acc, ps) = 
    case ps of
       [] => acc
     | p::ps' => foldl(f, f(p, acc), ps')

fun foldr f acc ps = 
    case ps of
       [] => acc
     | p::ps' => f(p, foldr f acc ps')

(* code skeleton for count_XX below *)
fun g f1 f2 p = 
    let 
      val r = g f1 f2
    in
      case p of 
          Wildcard => f1 ()
        | Variable x => f2 x        
        | ConstructorP(_, p) => r p
        | TupleP ps => foldl((fn (p, acc) => (r p) + acc), 0, ps)
        | _ => 0
    end

fun count_wildcards p = g (fn() => 1) (fn x => 0) p
fun count_wild_and_variable_lengths p = g (fn() => 1) (fn x => String.size x) p
fun count_some_var(arg_str, p) = g (fn() => 0) (fn str => if String.compare(str, arg_str) = General.EQUAL then 1 else 0) p

fun count_wildcards2 pattern = 
    let
      fun count_list_helper ps =
        case ps of
          [] => 0
        | p::ps' => count_wildcards p + count_list_helper ps'
    in
      case pattern of
          Wildcard => 1
        | ConstructorP(_, p) => count_wildcards p 
        | TupleP ps => count_list_helper ps            
        | _ => 0
    end

fun count_wild_and_variable_lengths2 pattern = 
    let
      fun count_list_helper ps =
        case ps of
          [] => 0
        | p::ps' => count_wild_and_variable_lengths p + count_list_helper ps'
    in
      case pattern of
          Wildcard => 1
        | Variable str => String.size(str)  
        | ConstructorP(_, p) => count_wild_and_variable_lengths p
        | TupleP ps => count_list_helper ps        
        | _ => 0
    end

fun count_some_var2(arg_str, pattern) = 
    let
      fun count_list_helper ps =
        case ps of
           [] => 0
         | p::ps' => count_some_var(arg_str, p) + count_list_helper ps'
    in
      case pattern of
        Variable str => if String.compare(str, arg_str) = General.EQUAL then 1 else 0
      | ConstructorP(_, p) => count_some_var(arg_str, p)
      | TupleP ps => count_list_helper ps
      | _ => 0
    end

fun check_pat p = 
    let
      fun check_pat_helper p = 
        case p of
          Variable str => str::[]
        | ConstructorP(_, p) => check_pat_helper p
        | TupleP ps => foldl((fn (p, acc) => (check_pat_helper p) @ acc), [], ps)
        | _ => []

      fun found_helper xs = 
        case xs of
           [] => false
         | target::xs' => if (List.exists (fn(x) => String.compare(x, target) = General.EQUAL) xs') then true
                          else found_helper xs'
    in
      if found_helper(check_pat_helper p) then false else true
    end


fun all_answers f xs = 
    let
      fun helper(xs, acc) = 
        case xs of
          [] => SOME(acc)
        | x::xs' => case f x of
            NONE => NONE
          | SOME(v) => helper(xs', acc @ v)
    in
      helper(xs, [])
    end

fun match(v, p) = 
    case (v, p) of
       (Constructor(sv, v'), ConstructorP(sp, p')) => if sv <> sp then NONE else match(v', p')
     | (Tuple(vlist), TupleP(plist)) => if List.length vlist <> List.length plist then NONE 
                                        else all_answers match (ListPair.zip(vlist, plist))
     | (_, Variable s) => SOME [(s, v)]
     | (Const a, ConstP b) => if a <> b then NONE else SOME []
     | (Unit, UnitP) => SOME []
     | (_, Wildcard) => SOME []
     | _ => NONE



fun first_answer f xs =
    let
      val g = first_answer f
    in
      case xs of 
        [] => raise NoAnswer
      | x::xs' => case f x of 
                      SOME(v) => v
                    | NONE => g xs'
    end

fun first_match v ps = 
    SOME(first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE   





fun only_capitals xs =
    filter(xs, fn x => Char.isUpper(String.sub(x, 0)))


fun longest_string1 xs = 
    foldl(fn (x, acc) => if String.size(x) > String.size(acc) then x else acc, "", xs)

fun longest_string2 xs = 
    foldl(fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc, "", xs)


fun longest_string_helper(compare_fn:int*int->bool) =
    fn xs => foldl(fn (x, acc) => if compare_fn(String.size x, String.size acc) then x else acc, "", xs)

fun greater(num1, num2) = 
    if num1 > num2 then true
    else false

fun greaterOrEqual(num1, num2) = 
    if num1 >= num2 then true
    else false

val longest_string3 = longest_string_helper greater
val longest_string4 = longest_string_helper greaterOrEqual


fun longest_capitalized(xs) = 
    longest_string1(only_capitals xs)


fun rev_string str = 
    let 
      val size = String.size str
      fun reverse_str(str, size) = 
        if size = 1 orelse size = 0 then str
        else reverse_str(String.substring(str, 1, size - 1), size - 1) ^ String.substring(str, 0, 1)
    in
      if size = 1 orelse size = 0 then str
      else reverse_str(str, size)
    end



(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []