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

fun only_capitals ls =
  List.filter (fn l => Char.isUpper(String.sub (l,0))) ls

fun longest_string1 ls =
  let 
    fun compare (s1,s2) = 
        if String.size s1 > String.size s2
        then s1
        else s2
  in
    List.foldl compare "" ls
  end

fun longest_string2 ls =
  let 
    fun compare (s1,s2) = 
        if String.size s1 >= String.size s2
        then s1
        else s2
  in
    List.foldl compare "" ls
  end

fun longest_string_helper f ls =
  let
    fun compare (s1,s2) =
      if f (String.size s1, String.size s2)
      then s1
      else s2
  in
    List.foldl compare "" ls
  end

val longest_string3 = fn ls =>
  (longest_string_helper (fn (size1,size2) => if size1 > size2 then
    true else false ) ls)

val longest_string4 = fn ls =>
  (longest_string_helper (fn (size1,size2) => if size1 >= size2 then
    true else false ) ls)

val longest_capitalized = fn ls =>
  (longest_string3 o only_capitals) ls

fun rev_string s =
  (implode o (rev o explode)) s

fun first_answer f l =
    case l of
         [] => raise NoAnswer
        |(x::lx') => case f x of
                          NONE => first_answer f lx' 
                         |SOME v => v

fun all_answers f l =
  let
    fun helper(f,l,result) =
      case l of
           [] => SOME result
         |x::lx' => case f x of
                         NONE => NONE
                       |SOME lst => helper(f,lx',lst@result)
  in
    helper(f,l,[])
  end

val count_wildcards =
  g (fn ()=> 1) (fn x => 0) 
      
val count_wild_and_variable_lengths =
  g (fn ()=> 1) String.size 

fun count_some_var (s,p) =
  g (fn () => 0) (fn a => if a = s then 1 else 0 ) p

fun check_pat p =
  let
    fun helper1 (p,ls) =
      case p of
           Wildcard => ls
         | Variable x => x::ls
         | TupleP pl => List.foldl helper1 ls pl
         | ConstructorP(_,x) => helper1 (x,ls)
         |_=>ls
    fun helper2 pl =
      case pl of
           [] => false
         | p::pl' => List.exists (fn x=> x=p) pl' orelse helper2 pl'
  in
    not (helper2 (helper1 (p,[])))
  end

fun match (v,p) =
  case (v,p) of
       (_,Wildcard) => SOME []
     | (v, Variable s) => SOME[(s,v)]
     | (Unit , UnitP ) => SOME[]
     | (Const _, ConstP _) => SOME[]
     | (Tuple vl, TupleP pl) => if List.length vl = List.length pl
                               then all_answers (match) (ListPair.zip(vl,pl))
                               else NONE
     |(Constructor (s1,v),ConstructorP (s2,p)) => if s1 = s2
                                                  then match (v,p)
                                                  else NONE
     | _ => NONE

fun first_match v pl =
  SOME (first_answer (fn p => match(v,p)) pl)
  handle NoAnswer => NONE
