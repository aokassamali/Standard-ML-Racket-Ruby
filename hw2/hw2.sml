(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (a, b) = 
    case b of
        [] => NONE
      | b::bs =>
          if same_string(a, b) then SOME bs
          else case all_except_option(a, bs) of
                 NONE => NONE
               | SOME bs' => SOME (b::bs')

fun get_substitutions1 (b, s) =
    case b of
	[] => NONE 
      | b::bs => all_except_option(s,b) 
					  
(* fun get_substitutions1 ([], s) = []
  | get_substitutions1 (a::rest,s)  =
    case all_except_option(s,a) of
	NONE => get_substitutions1(rest,s) 
      | SOME [] => a @ rest *) 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
