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


(* Problem 1 *)

fun only_capitals(s_list) =
	List.filter(fn x => Char.isUpper(String.sub(x,0))) s_list

(* Problem 2 *)

fun longest_string1(s_list) =
	let
		fun s_compare(a,b) =
			if String.size(a) > String.size(b)
			then a
			else b
	in
		foldl(s_compare)("") s_list
	end

(* Problem 3 *)

fun longest_string2(s_list) =
	let
		fun s_compare(a,b) =
			if String.size(a) >= String.size(b)
			then a
			else b
	in
		foldl(s_compare)("") s_list
	end

(* Problem 4 *)

fun longest_string_helper(f) = 
	let
		fun s_compare(a,b) =
			if f(String.size(a),String.size(b))
			then a
			else b
	in
		foldl(s_compare)("")
	end

val longest_string3 = longest_string_helper(fn (a,b) => a > b)
val longest_string4 = longest_string_helper(fn (a,b) => a >= b)

(* Problem 5 *)

val longest_capitalized = longest_string3 o only_capitals

(* Problem 6 *)

val rev_string = implode o rev o explode

(* Problem 7 *)

fun first_answer(f) =
	let fun g(a_list) =
		case a_list of
			[] => raise NoAnswer
			| x::more => case f(x) of
							NONE => g(more)
							| SOME v => v
	in g
	end

(* Problem 8 *)

fun all_answers(f) =
	let fun g(acc, a_list) =
		case a_list of
			[] => SOME acc
			| a::more => case f(a) of
							NONE => NONE
							| SOME lst => g(acc @ lst, more)
	in
		fn x => g([],x)
	end