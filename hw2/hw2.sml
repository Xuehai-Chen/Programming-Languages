(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, lst) =
let fun helper (str, x, xs')=
         if same_string(str, x)
         then SOME(xs')
         else case all_except_option(str,xs') of
                  NONE => NONE
                 |SOME(xs'') => SOME(x::xs'') 
in
  case lst of
       [] => NONE
     | x::xs' => helper(str,x,xs')
end

fun get_substitutions1( substitutions, s) =
  case substitutions of 
       [] => []
     | x::xs' => 
        case all_except_option(s,x) of
             NONE => get_substitutions1(xs',s) 
           |SOME(xs'')=> xs''@ get_substitutions1(xs',s)

fun get_substitutions2 (sub, s) =
let fun helper(sub,s,res) =
case sub of
     [] => res
   |x::xs' =>
       case all_except_option(s,x) of
            NONE => helper(xs',s,res)
          |SOME(xs'') => helper(xs',s,res@xs'')
in
  helper(sub,s,[])
end

fun similar_names ( substitutions, name) =
let
  val { first=f, middle=m, last=l} = name
  fun helper(firsts,{middle,last},lst) =
    case firsts of
          []=> lst
        | x::xs =>
            helper(xs,{middle=m,last=l},lst@[{first=x,middle=m,last=l}])
in
  helper(get_substitutions2(substitutions,f),{middle=m,last=l},[{first=f,middle=m,last=l}])
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

fun card_color (card) =
  case card of
       (Clubs,_) => Black
     | (Spades,_) => Black
     | (Diamonds,_) => Red
     | (Hearts,_) => Red

fun card_value (card) =
  case card of
       (_,Num i) => i
     | (_,Ace) => 11
     | (_,King) => 10
     | (_,Queen) => 10
     | (_,Jack) => 10

fun remove_card (cs,c,e) =
  let fun helper(cs,c,lst,e) =
  case cs of
       [] => raise e
     |cs'::cs'' => 
         if cs'= c
         then cs''@lst
         else helper(cs'',c,cs'::lst,e)
  in
    helper(cs,c,[],e)
  end
  
fun all_same_color (cs) =
  let 
    fun helper(cs,color) =
     case cs of 
         [] => true
         | c::cs' =>
             if card_color(c) = color
             then helper(cs',color)
             else false
  in
    case cs of
         [] => true
       |c::cs'' => helper(cs'',card_color(c))
  end
    
fun sum_cards (cs) =
  let 
    fun helper (cs,sum) =
      case cs of
           []=> sum
         | c::cs' => helper(cs',card_value(c)+sum)
  in
    helper(cs,0)
  end

fun score (cs,goal) =
  let 
    val sum = sum_cards(cs)
  in
    let val preliminary = 
    if sum > goal then 3*(sum - goal)
    else goal - sum
    in
      if all_same_color(cs) 
      then preliminary div 2
      else preliminary
    end
  end

fun officiate (cs, ms, goal) =
  let 
    fun helper(helds, cs, ms, goal) =
    let val score = score(helds,goal)
    in
      if score > goal then score
      else
        case ms of
             []=> score
            | m::ms' => 
                 case m of
                     Discard card=>
                        helper(remove_card(helds,card,IllegalMove),cs,ms',goal)
                    |Draw => 
                        case cs of 
                            []=> score
                            |c::cs' => helper (c::helds,cs',ms',goal)
    end
  in
    helper([],cs,ms,goal)
  end
