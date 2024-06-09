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

(*fun similar_names (b, r) =
    case r of
	{first,last,middle} => let val acc = [r]
				   fun help (x,acc) =
				       case x of
					   [] => acc
					 | x::xs => [{first=x ,last=last ,middle=middle}] @ help(xs,acc)
			       in
				   help(get_substitutions2(b,first),acc)
			       end*)

fun similar_names (b, r) =
    case r of
        {first, last, middle} =>
            let
                val acc = [r]
                fun help (x, acc) =
                    case x of
                        [] => acc
                      | x::xs => help(xs, acc @ [{first=x, last=last, middle=middle}])
                val result = help(get_substitutions2(b, first), acc)
            in
                result
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
      | b::bs => if b = c then bs
                 else b:: remove_card(bs, c, e)
              

      
  fun all_same_color b =
      case b of
	  [] => true 
	| (b1,b2)::(b3,b4)::bs => (card_color(b1,b2) = card_color(b3,b4)) andalso all_same_color((b3,b4)::bs)
	| [(_,_)] => true


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
	      true => prelim_score(b,g) div 2
	    | false => prelim_score(b,g)
      end
	      

fun officiate (b, m, g) =
    let
        fun game_state (h, m, b) =
            case m of
                [] => score (h, g)
              | m::ms => (case m of
                            Discard c => (case h of
                                           [] => raise IllegalMove
                                         | _ => game_state (remove_card (h, c, IllegalMove), ms, b))
                          | Draw => (case b of
                                      [] => score (h, g)
                                    | b::bs => if sum_cards (b::h) > g
                                               then score (b::h, g)
                                               else game_state (b::h, ms, bs)))
    in
        game_state ([], m, b)
    end


fun num_aces (b, acc) =
    case b of
	[] => acc
      | (_,Ace)::bs => num_aces(bs, acc + 1)
      | _::bs => num_aces (bs, acc)
	
fun score_challenge (b,g) =
    let
	val sum = sum_cards b
	val aces = num_aces(b,0)
	fun prelim_score (b, g) =
	    if sum > g andalso aces > 0 andalso (sum - 10*aces) > g
	    then 3 * ((sum - 10*aces)-g)
	    else if sum > g andalso aces > 0 andalso (sum - 10*aces) < g
	    then g - (sum - 10*aces)
	    else if sum > g andalso aces <= 0
	    then 3*(sum-g)
	    else (g-sum)
    in
	case all_same_color b of
	    true => prelim_score (b,g) div 2
	  | false => prelim_score (b,g)
    end



fun officiate_challenge (b, m, g) =
    let 
        fun game_state (h, m, b) =
            case m of
                [] => score_challenge (h, g)
              | m::ms => (case m of
                            Discard c => (case h of
                                           [] => raise IllegalMove
                                         | _ => game_state (remove_card (h, c, IllegalMove), ms, b))
                          | Draw => (case b of
                                      [] => score_challenge (h, g)
                                    | b::bs => if sum_cards (b::h) > g andalso num_aces(h,0) > 0 andalso (sum_cards (b::h) - 10*num_aces(h,0)) > g
                                               then score_challenge (b::h, g)
                                               else game_state (b::h, ms, bs)))
    in
        game_state ([], m, b)
    end


fun careful_player (b, g) =
    let
	fun move_list (h, b, m) =
	    case b of
		[] => Draw::m
	      | b1::[] => if g - (card_value b1 + sum_cards h) > 10 then move_list(b1::h, [], Draw::m)
			  else if (card_value b1 + sum_cards h) <= g then move_list(b1::h, [], Draw::m)
			  else m
	      | b1::b2::bs' => if g - (card_value b1 + sum_cards h) > 10 then move_list(b1::h, b2::bs', Draw::m)
			       else if (card_value b1 + sum_cards h) = g then move_list(b1::h, [], Draw::m)
			       else if ((card_value b2 + sum_cards h) = g) then move_list(b1::b2::h, [],Draw::Discard b1::Draw::m)
			       else if (card_value b1 + sum_cards h) < g then move_list(b1::h, b2::bs', Draw::m)
			       else if ((card_value b2 + sum_cards h) < g) then move_list(b1::b2::h, bs',Draw::Discard b1::Draw::m)
			       else m
    in
	move_list([],b,[])
    end
												    
