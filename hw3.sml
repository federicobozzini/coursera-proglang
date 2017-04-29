(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

infix |> (* tells the parser |> is a function that appears between its two arguments *)
fun x |> f = f x



val only_capitals = List.filter (fn x => String.sub(x, 0) |> Char.isUpper)

val longest_string1 = List.foldl (fn (x, acc) => if String.size(x) > String.size(acc) then x else acc) ""

val longest_string2 = List.foldl (fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) ""

fun longest_string_helper comp = List.foldl (fn (x, acc) => if comp(x, acc) then x else acc) "" 

val longest_string3 = longest_string_helper (fn (x, acc) => String.size(x) > String.size(acc))

val longest_string4 = longest_string_helper (fn (x, acc) => String.size(x) >= String.size(acc))

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f xs = 
        List.map f xs |>
        List.filter isSome |> 
        (fn xs => case xs of
                    [] => raise NoAnswer 
                  | x::_ => valOf(x) )
                  
fun all_answers f xs =
        List.map f xs |>
        (fn xs => if List.all isSome xs
                  then List.map (valOf) xs |> List.concat|> (fn l => SOME l)
                  else NONE)
                  
val count_wildcards = g (fn () => 1) (fn x => 0)  
                  
val count_wild_and_variable_lengths = g (fn () => 1) String.size  

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat(p) =
    let fun get_ids(p) = 
        case p of
	      Variable x        => [x]
	    | TupleP ps         => List.map get_ids ps |> List.concat
	    | ConstructorP(_,p) => get_ids(p)
	    | _                 => []
	    val ids = get_ids(p)
	    val unique_ids = List.foldr (fn (x, acc) => if List.exists (fn y => x=y) acc then acc else x::acc) [] ids
	in  
	    ids = unique_ids  
	end  
	
fun match(v, p) =
    case (v, p) of
      (_, Wildcard) => SOME []
    | (v, Variable(s)) => SOME [(s,v)]
    | (Unit, UnitP) => SOME []
    | (Const(x), ConstP(y)) => if x=y then SOME [] else NONE
    | (Tuple(vs), TupleP(ps)) => 
            if (List.length vs = List.length ps)
            then ListPair.zip(vs, ps) |> all_answers match
            else NONE
    | (Constructor(s1, v1), ConstructorP(s2, p1)) => 
            if (s1=s2) 
            then match(v1, p1) 
            else NONE
    | _ => NONE
    
fun first_match v ps = 
    first_answer (fn p => match(v,p)) ps  |> SOME 
    handle NoAnswer => NONE
