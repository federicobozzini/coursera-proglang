(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)        
    
fun all_except_option(s,ss) = 
    case ss of
      []     => NONE
    | s1::ss1 => if same_string(s, s1) 
                 then SOME(ss1)
                 else
                    case all_except_option(s,ss1) of
                      NONE => NONE
                    | SOME(r) => SOME(s1::r)    

fun get_substitutions1(sss, s1) = 
    case sss of
      [] => []
    | ss::sss2 =>
        (case all_except_option(s1, ss) of
          NONE => []
        | SOME l => l)
        @ get_substitutions1(sss2, s1)
        
        
fun get_substitutions2(sss, s1) = 
    let fun f(sss, s1, acc) = 
    case sss of
      [] => acc
    | ss::sss2 =>
        f(sss2, s1, acc @ (case all_except_option(s1, ss) of
          NONE => []
        | SOME l => l))
    in
        f(sss, s1, [])
    end
    

fun similar_names(sss, nameRec) =
    let val {first=f, middle=m, last= l} = nameRec
        val firstNames = get_substitutions1(sss, f)
        fun createRecords(firstNames) =
         case firstNames of
           [] => []
         | name::otherNames => {first=name, middle=m, last=l} :: createRecords(otherNames)
    in
        nameRec :: createRecords(firstNames)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(s, r) = 
    case s of
      Clubs => Black
    | Spades => Black
    | Diamonds => Red
    | Hearts => Red

fun card_value(s, r) = 
    case r of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num(i) => i
    
fun remove_card(cs, c, e) = 
    case cs of
      [] => raise e
    | c1::cs1 => 
        if c1 = c
        then cs1
        else c1 :: remove_card(cs1, c, e)
        
fun all_same_color(cs) =
    case cs of
      [] => true
    | _::[] => true
    | c::(c1::rest) => (card_color(c) =card_color(c1) andalso all_same_color (c1::rest))
        
fun sum_cards(cs) =
    let fun s(cs, acc) = 
        case cs of
          [] => acc
        | c::cs1 => s(cs1, acc+card_value(c))
    in
        s(cs, 0)
    end
    
fun score(cs, goal) = 
    let val sum = sum_cards(cs)
        val preliminary_score =  if (sum > goal) then 3*(sum-goal) else goal-sum
    in
        if (all_same_color(cs))
        then preliminary_score div 2
        else preliminary_score
    end
    
fun officiate(cs, moves, goal) = 
    let fun process(cs, hs, moves) =
        case moves of
          [] => score(hs, goal)
        | Draw::ms => (
            case cs of
              [] => score(hs, goal)
            | c1::cs1 => 
                let val hs1 = c1::hs in
                    if sum_cards(hs1) > goal
                    then score(hs1, goal)
                    else process(cs1, hs1, ms) 
                end )
        | Discard(c)::ms => process(cs, remove_card(hs, c, IllegalMove), ms)
    in
        process(cs, [], moves)
    end
