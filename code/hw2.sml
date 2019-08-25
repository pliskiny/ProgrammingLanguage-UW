datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun same_string(s1:string, s2:string) = 
    s1 = s2


fun all_except_option(str_list:string list, target:string) = 
    case str_list of
        [] => NONE
      | x::str_list_ => 
            if same_string(x, target) then SOME(str_list_)
            else 
                case all_except_option(str_list_, target) of
                    SOME(i) => SOME(x::i)
                  | NONE => NONE

fun get_substitutions2(subtitutions:string list list, target:string) =
    let 
        fun get_substitutions2_helper(subtitutions:string list list, target:string, acc:string list) = 
            case subtitutions of
                [] => acc
              | str_list::subtitutions_ => 
                    case all_except_option(str_list, target) of
                        NONE => get_substitutions2_helper(subtitutions_, target, acc)
                    | SOME(i) => get_substitutions2_helper(subtitutions_, target, acc @ i)
    in
        get_substitutions2_helper(subtitutions, target, [])
    end

fun similar_names(subtitutions:string list list, name:{first:string, last:string, middle:string}) =
    let
        val {first=f_name, last=l_name, middle=m_name} = name
        val xs = get_substitutions2(subtitutions, f_name)
        fun similar_names_helper(xs, l_name, m_name) = 
            case xs of
                [] => []
              | x::xs_ =>                         
                    {first=x, last=l_name, middle=m_name}::similar_names_helper(xs_, l_name, m_name)
    in  name::similar_names_helper(xs, l_name, m_name)
    end



fun card_color(c:card) =
    case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (_, _) => Red


fun card_value(c:card) =
    case c of
        (_, Num v) => v  
      | (_, Ace) => 11
      | _ => 10
    

fun remove_card(cs:card list, c:card) = 
    let 
        fun remove_card_helper(xs:card list, c:card) = 
            case xs of
                [] => NONE
              | x::xs_ => 
                    if x = c then SOME(xs_)
                    else 
                        case remove_card_helper(xs_, c) of
                            NONE => raise IllegalMove
                          | SOME(i) => SOME(x::i)
    in
        case remove_card_helper(cs, c) of
            SOME(i) => i
    end


fun all_same_color(xs:card list) = 
    case xs of
        [] => true
      | [_] => true
      | x1::x2::xs_ => card_color(x1) = card_color(x2) andalso all_same_color(x2::xs_)


fun sum_cards(cs:card list) =
    let 
        fun helper(cs, acc) = 
            case cs of
                [] => acc
              | c::cs_ => helper(cs_, card_value(c) + acc)
    in
        helper(cs, 0)
    end


fun score(held_cards:card list, goal:int) = 
    let 
        val sum = sum_cards(held_cards)
    in
        let
            val p_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
        in
            if all_same_color(held_cards) then p_score div 2
            else p_score 
        end    
    end


fun officiate(cs:card list, moves:move list, goal:int) =
    let
        fun officiate_helper(moves:move list, goal:int, held_cards:card list, cs:card list) = 
            case moves of
                [] => held_cards
              | m::moves_ => 
                    case m of 
                        Discard(c) => officiate_helper(moves_, goal, remove_card(held_cards, c), cs)
                      | Draw => 
                            case cs of 
                                [] => held_cards
                              | c::cs_ => 
                                    let 
                                        val held_cards_ = c::held_cards
                                    in
                                        if sum_cards(held_cards_) > goal then held_cards_
                                        else officiate_helper(moves_, goal, held_cards_, cs_)
                                    end
    in  
        score(officiate_helper(moves, goal, [], cs), goal)
    end
    


val test1 = all_except_option (["string"], "string") = SOME []
val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black
val test6 = card_value (Clubs, Num 2) = 2
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace)) = []
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)