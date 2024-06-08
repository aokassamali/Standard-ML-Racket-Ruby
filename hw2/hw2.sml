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
	[] => [] 
      | b::bs' => case all_except_option(s,b) of
		     SOME bs => bs @ get_substitutions1(bs',s)
		   | NONE => get_substitutions1(bs',s)

fun get_substitutions2 (b,s) =
    let
	fun aux (b, acc) =
	    case b of
		[] => acc 
	      | b::bs' => case all_except_option(s,b) of
			      SOME bs => bs @ aux(bs' ,acc)
			    | NONE => aux(bs', acc) 
    in
	aux(b,[])
    end

fun similar_names (b, r) =
    case r of
       {first,last,middle} => let val acc = [r]
			  fun help (x,acc) =
			      case x of
				  [] => acc
				| x::xs => [{first=x ,last=last ,middle=middle}] @ help(xs,acc)
		      in
			  help(get_substitutions2(b,first),acc)
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

fun card_color card =
    case card of
	(Clubs,_) =>  Black
      | (Spades,_) =>  Black
      | _  => Red

  fun card_value card =
    case card of
	(_,Num n) => n
      | (_,Ace) => 11
      | _ => 10

  fun remove_card (b, c, e) =
      case b of
	  [] => raise e
	 |b::bs => if b=c then bs
		   else case remove_card(bs,c,e) of
			    [] => raise e
			  | bs' => b::bs' 	 
      
  fun all_same_color b =
      case b of
	  [] => true
	| [(_,_)] => true 
	| (b1,b2)::(b3,b4)::bs => (card_color(b1,b2) = card_color(b3,b4)) andalso all_same_color(bs)



  fun sum_cards b =
      let
	  fun aux (b,acc) =
	      case b of
		  [] => acc
		| i::bs' => aux(bs',card_value(i) + acc)
      in
	  aux(b,0)
      end

  fun score (b, g) =
      let
	  val sum = sum_cards b
	  fun prelim_score (b, g) =
	      if sum > g then 3*(sum-g) else (g-sum)
      in
	  case all_same_color(b) of
	      true => prelim_score(b,g)
	    | false => prelim_score(b,g) div 2
      end
	      
